000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400121227     Ftabel00f  if   e           k disk
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
001700121227     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700121227     D FlgNewBol       s              1    INZ(*blanks)
002800121227     D jNAZ            s              5  0 INZ(*zeros)
002900121227
003000121227     D*------------------
003100121227     D* DS REPERIMENTO NUMERATORE
003200121227     D*------------------
003300121227     D trul33ds      e ds                  inz
003400121227     D*------------------
003500121227     D* DS ARCHITETTURA
003600121227     D*------------------
003700121227     D kpjba         e ds                  inz
003800121227     D*------------------
003900121227
004000121227     D*------------
004100121227     D* SCHIERE
004200121227     D*------------
004300121227     D skNAZISO        S              3    DIM(1000)
004400121227     D skNAZBAR        S              3    DIM(1000)
004500090113
004600090113     D*------------------
004700090113     D* LINKING A DEFINIZIONI ESTERNE
004800090113     D*------------------
004900090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005000090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005100090113
005200010201
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000613     C*
005700121227     C                   EXSR      CARTAB                                       CARICA TABELLE
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
008700070103     C*
008800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008900020305     C                   exsr      prevat
009000010201     C*
009100010202     C                   if        chkcall = '0'
009200010202     C*
009300021113     C                   if        not %open(fivatwwr)
009400021113     C                   open      fivatwwr
009500010201     C                   endif
009600990910     C*
009700010201     C                   clear                   §CTROKVB          5 0
009800020305     C                   clear                   §CTROKVT          5 0
009900000801     C                   clear                   §CTRMO            5 0
010000000801     C                   clear                   §CTRNO            5 0
010100990910     C*
010200921023     C                   DO        *HIVAL
010300990913     C*
010400990915     C                   READ      tivin00r                               70
010500050627     C                   if        vindta > *blanks
010600000613     C                   add       1             rrnum
010700000801     C*
010800000801     C                   if        *in70 = *off
010900000801     C                             and
011000000801     C                             (vinflg = *blanks
011100000801     C                              or vinflg = '0'
011200000801     C                              or vinflg = '2')
011300000801     C*
011400000801     C                   clear                   vinmsg
011500000801     C                   eval      vinflg = '1'
011600121227     C*
011700121227     C* Eseguo routine d traduzione
011800121227     C                   exsr      impvabvat
011900000905     C*
012000000905     C                   else
012100000905     C                   eval      vinflg = '1'
012200050628     C                   endif
012300000905     C                   endif
012400000905     C*
012500000905     C  N70              update    tivin000
012600000905     C*
012700991022     C  N70              ENDdo
012800121227     C*
012900121227     C* Scarico i buffer eventualmente rimasti in sospeso
013000121227     C                   IF        FlgNewBol = '1'
013100121227     C                   EXSR      wrivab
013200121227     C                   ENDIF
013300010202     C*
013400010202     C                   endif
013500990910
013600990910     C* Se non ci sono record con errori ...
013700000710     C                   if        §ctrno = 0
013800990910     C* ... restituisco esito OK.
013900990921     C                   eval      wrkesito = '0'
014000990910     C                   else
014100010201     C                   if        §ctrokvb > 0
014200990921     C                   eval      wrkesito = '1'
014300000710     C                   else
014400000710     C                   eval      wrkesito = '2'
014500990910     C                   endif
014600000710     C                   endif
014700990910     C*
014800990914     C                   if        %open(tivin00r)
014900990908     C                   close     tivin00r
015000990914     C                   endif
015100021113     C                   if        %open(fivabwwr)
015200021113     C                   close     fivabwwr
015300990914     C                   endif
015400021113     C                   if        %open(fivatwwr)
015500021113     C                   close     fivatwwr
015600010201     C                   endif
015700990910     C*
015800010201     C                   if        §ctrokvb > 0
015900000724     C                             and vlrpoi <> *zeros
016000010202     C                   exsr      invio
016100990920     C                   endif
016200990920     C*
016300910830     C                   ENDSR
016400000613     C***
016500010305
016600010305     C*----------------------------------------------------*
016700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016800010305     C*----------------------------------------------------*
016900020305     C     WRIVAB        BEGSR
017000010305     C*
017100060225     C* Quindi scarico il buffer del file d testata
017200021113     C                   write     fivab000                                     => scarico il VAB
017300010305     C*
017400010305     C                   ENDSR
017500990920
017600000801     C*----------------------------------------------------*
017700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017800000801     C*----------------------------------------------------*
017900010201     C     INZVAR        BEGSR
018000000801     C*
018100070213     C                   CLEAR                   FIVAB000
018200070213     C                   CLEAR                   FIVAT000
018300070213     C*
018400040802     C                   Z-ADD     *zeros        Num5_0            5 0
018500040802     C                   MOVEL     '0'           FlgCAS            1
018600000801     C*
018700000801     C                   ENDSR
018800000801     C*----------------------------------------------------*
018900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019000000801     C*----------------------------------------------------*
019100000801     C     DEFCAM        BEGSR
019200000801     C*
019300020619     C* Imposto i valori di default...
019400121227     C                   Z-ADD     0030325       VABCCM
019500121227     C                   Z-ADD     0030325       VATCCM
019600121227     C                   Z-ADD     003           VABLNP
019700121227     C                   Z-ADD     003           VATLNP
019800090113     C                   Z-ADD     000           VABCTR
019900130115     C***                MOVEL     '7Q'          VABCTM
020000040823     C                   MOVEL     '1'           VABCBO
020100020619     C* ... e poi verifico se sono stati passati come parametri
020200020619     C                   IF        vlrppt > *blanks
020300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020500020619     C                   EXSR      CHKNUM
020600020619     C                   IF        PiInt=*on
020700020619     C                   Z-ADD     PiVal         VABCCM
020800020619     C                   Z-ADD     PiVal         VATCCM
020900020619     C                   ENDIF
021000040506     C                   ENDIF
021100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABLNP
021600020619     C                   Z-ADD     PiVal         VATLNP
021700040506     C                   ENDIF
021800020619     C                   ENDIF
021900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCTR
022400040506     C                   ENDIF
022500130315     C                   ENDIF
022600130315     C                   IF        %subst(vlrppt:14:2) <> *blanks
022700130315     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
022800020619     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200121227     C*----------------------------------------------------*
023300121227     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
023400121227     C*----------------------------------------------------*
023500121227     C     IMPVABVAT     BEGSR
023600121227     C*
023700121227     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
023800121227     C                   Z-ADD     *zeros        errore            1 0
023900121227     C********
024000121227     C* Tipo record 'A'
024100121227     C********
024200121227     C                   IF        %trim(%subst(vindta:1:1)) = 'A'
024300121227     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
024400121227     C                   IF        FlgNewBol = '1'
024500121227     C                   EXSR      wrivab
024600121227     C                   EVAL      FlgNewBol = '0'
024700121227     C                   ENDIF
024800121227     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
024900121227     C                   EXSR      INZVAR
025000121227     C                   EXSR      DEFCAM
025100121227     C* ......valorizzo flag "nuova bolla"
025200121227     C                   EVAL      FlgNewBol = '1'
025300121227     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
025400121227     C                   MOVEL     datcor        VABAAS
025500121227     C                   MOVEL     datcor        VATAAS
025600121227     C                   MOVE      datcor        VABMGS
025700121227     C                   MOVE(P)   vlrpoi        VABFGS
025800121227     C                   MOVE(P)   vlrpoi        VATFGS
025900121227     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
026000121227     C                   CLEAR                   TRUL33DS
026100121227     C                   EVAL      I33OPE = *zeros
026200121227     C                   EVAL      I33CNU = 302
026300121227     C                   EVAL      I33NUM = 1
026400121227     C                   MOVEL     TRUL33DS      KPJBU
026500121227     C                   CALL      'TRUL33R'
026600121227     C                   PARM                    KPJBA
026700121227     C                   MOVEL     KPJBU         TRUL33DS
026800121227     C                   IF        O33ERR = *zeros
026900121227     C                   Z-ADD     O33NRF        VABNSP
027000121227     C                   Z-ADD     O33NRF        VATNSP
027100121227     C                   ELSE
027200121227     C                   ADD       1             errore
027300121227     C                   EVAL      vinmsg = %trimr(vinmsg)
027400121227     C                             + ' ' + 'VABNSP VATNSP'
027500121227     C                   ENDIF
027600121227     C* ......VABRMA
027700121227     C                   EVAL      VABRMA=%trim(%subst(vindta:7:18))
027800121227     C* ......VABRMN
027900121227     C                   EVAL      PiStr=%trim(%subst(vindta:7:18))
028000121227     C                   EXSR      CHKNUM
028100121227     C                   IF        PiInt=*on
028200121227     C                   Z-ADD     PiVal         VABRMN
028300121227     C                   ELSE
028400121227     C                   Z-ADD     1             VABRMN
028500121227     C                   ADD       1             errore
028600121227     C                   EVAL      vinmsg = %trimr(vinmsg)
028700121227     C                             + ' ' + 'VABRMN'
028800121227     C                   ENDIF
028900121227     C* ......VABRSD
029000121227     C                   EVAL      VABRSD=%trim(%subst(vindta:322:30))
029100121227     C* ......VABRD2
029200121227     C                   EVAL      VABRD2=%trim(%subst(vindta:382:30))
029300121227     C* ......VABIND
029400121227     C                   EVAL      VABIND=%trim(%subst(vindta:352:30))
029500121227     C* ......VABLOD
029600121227     C                   EVAL      VABLOD=%trim(%subst(vindta:412:30))
029700121227     C* ......VABPRD
029800121227     C                   EVAL      VABPRD=%trim(%subst(vindta:442:30))
029900121227     C* ......VABCAD
030000121227     C                   EVAL      PiStr=%trim(%subst(vindta:472:9))
030100121227     C                   EXSR      CHKNUM
030200121227     C                   IF        PiInt=*on
030300121227     C                   Z-ADD     PiVal         Num5_0
030400121227     C                   MOVEL(P)  Num5_0        VABCAD
030500121227     C                   ELSE
030600121227     C                   ADD       1             errore
030700121227     C                   EVAL      vinmsg = %trimr(vinmsg)
030800121227     C                             + ' ' + 'VABCAD'
030900121227     C                   ENDIF
031000121227     C* ......VABNZD
031100121227     C                   EVAL      VABNZD=%trim(%subst(vindta:481:3))
031200121227     C                   IF        VABNZD='IT'
031300121227     C                   EVAL      VABNZD=*blanks
031400121227     C                   ELSE
031500121227     C                   Z-ADD     1             jNAZ
031600121227     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
031700121227     C                   IF        %found
031800121227     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
031900121227     C                   ENDIF
032000121227     C                   ENDIF
032100121227     C* ......VABPRD (reperita mediante TISI95)
032200121227     C                   IF        VABCAD <> *blanks AND
032300121227     C                             VABNZD =  *blanks
032400121227     C                   CLEAR                   TISI95DS
032500121227     C                   EVAL      I95TCN = '3'
032600121227     C                   Z-ADD     datcor        I95DAT
032700121227     C                   EVAL      I95CAP = VABCAD
032800121227     C                   EVAL      I95LOC = VABLOD
032900121227     C                   CALL      'TISI95R'
033000121227     C                   PARM                    TISI95DS
033100121227     C                   EVAL      VABPRD = O95PRV
033200121227     C                   ENDIF
033300121227     C* ......VATNOT_A (referente x consegna)
033400121227     C                   EVAL      VATNOT=%trim(%subst(vindta:504:22))
033500121227     C                   EXSR      exevata
033600121227     C* ......VATNOT_B (telefono x consegna)
033700121227     C                   EVAL      VATNOT=%trim(%subst(vindta:526:7))+' '+
033800121227     C                                    %trim(%subst(vindta:533:9))
033900121227     C                   EXSR      exevatb
034000121227     C*
034100121227     C**** GESTIONE LUOGO DI CONSEGNA (opzionale)
034200121227     C                   IF        %subst(vindta:841:267) <> *blanks
034300121227     C* ......VABRSD
034400121227     C                   EVAL      VABRSD=%trim(%subst(vindta:856:30))
034500121227     C* ......VABRD2
034600121227     C                   EVAL      VABRD2=%trim(%subst(vindta:916:30))
034700121227     C* ......VABIND
034800121227     C                   EVAL      VABIND=%trim(%subst(vindta:886:30))
034900121227     C* ......VABLOD
035000121227     C                   EVAL      VABLOD=%trim(%subst(vindta:946:30))
035100121227     C* ......VABPRD
035200121227     C                   EVAL      VABPRD=%trim(%subst(vindta:976:30))
035300121227     C* ......VABCAD
035400121227     C                   EVAL      PiStr=%trim(%subst(vindta:1006:9))
035500121227     C                   EXSR      CHKNUM
035600121227     C                   IF        PiInt=*on
035700121227     C                   Z-ADD     PiVal         Num5_0
035800121227     C                   MOVEL(P)  Num5_0        VABCAD
035900121227     C                   ELSE
036000121227     C                   ADD       1             errore
036100121227     C                   EVAL      vinmsg = %trimr(vinmsg)
036200121227     C                             + ' ' + 'VABCAD'
036300121227     C                   ENDIF
036400121227     C* ......VABNZD
036500121227     C                   EVAL      VABNZD=%trim(%subst(vindta:1015:3))
036600121227     C                   IF        VABNZD='IT'
036700121227     C                   EVAL      VABNZD=*blanks
036800121227     C                   ELSE
036900121227     C                   Z-ADD     1             jNAZ
037000121227     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
037100121227     C                   IF        %found
037200121227     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
037300121227     C                   ENDIF
037400121227     C                   ENDIF
037500121227     C* ......VABPRD (reperita mediante TISI95)
037600121227     C                   IF        VABCAD <> *blanks AND
037700121227     C                             VABNZD =  *blanks
037800121227     C                   CLEAR                   TISI95DS
037900121227     C                   EVAL      I95TCN = '3'
038000121227     C                   Z-ADD     datcor        I95DAT
038100121227     C                   EVAL      I95CAP = VABCAD
038200121227     C                   EVAL      I95LOC = VABLOD
038300121227     C                   CALL      'TISI95R'
038400121227     C                   PARM                    TISI95DS
038500121227     C                   EVAL      VABPRD = O95PRV
038600121227     C                   ENDIF
038700121227     C* ......VATNOT_A (referente x consegna)
038800121227     C                   EVAL      VATNOT=%trim(%subst(vindta:1038:22))
038900121227     C                   EXSR      exevata
039000121227     C* ......VATNOT_B (telefono x consegna)
039100121227     C                   EVAL      VATNOT=%trim(%subst(vindta:1060:7))+' '+
039200121227     C                                    %trim(%subst(vindta:1067:9))
039300121227     C                   EXSR      exevatb
039400121227     C
039500121227     C                   ENDIF
039600121227     C****
039700121227     C*
039800121227     C                   ENDIF
039900121227     C*
040000121227     C********
040100121227     C* Tipo record 'B'
040200121227     C********
040300121227     C                   IF        %trim(%subst(vindta:1:1)) = 'B'
040400121227     C* ......VABNOT
040500121227     C                   EVAL      VABNOT=%trim(%subst(vindta:43:30))
040600121227     C* ......VABNT2
040700121227     C                   EVAL      VABNT2=%trim(%subst(vindta:43+30:60-30))
040800121227     C* ......VABCBO
040900121227     C                   IF        %subst(vindta:128:1) = 'R'
041000121227     C                   EVAL      VABCBO = '2'
041100121227     C                   ELSE
041200121227     C                   EVAL      VABCBO = '1'
041300121227     C                   ENDIF
041400121227     C* ......VABNCL
041500121227     C                   EVAL      PiStr=%trim(%subst(vindta:141:4))
041600121227     C                   EXSR      CHKNUM
041700121227     C                   IF        PiInt=*on
041800121227     C                   Z-ADD     PiVal         VABNCL
041900121227     C                   ELSE
042000121227     C                   Z-ADD     *zeros        VABNCL
042100121227     C                   ADD       1             errore
042200121227     C                   EVAL      vinmsg = %trimr(vinmsg)
042300121227     C                             + ' ' + 'VABNCL'
042400121227     C                   ENDIF
042500121227     C* ......VABPKB
042600121227     C                   EVAL      PiStr=%trim(%subst(vindta:145:8))
042700121227     C                   EXSR      CHKNUM
042800121227     C                   IF        PiNum=*on
042900121227     C                   ADD(H)    PiVal         VABPKB
043000121227     C                   ELSE
043100121227     C                   ADD       1             errore
043200121227     C                   EVAL      vinmsg = %trimr(vinmsg)
043300121227     C                             + ' ' + 'VABPKB'
043400121227     C                   ENDIF
043500121227     C* ......VABVLB
043600130315     C                   IF        %subst(vindta:178:7) <> *blanks
043700121227     C                   EVAL      PiStr=%trim(%subst(vindta:178:7))
043800121227     C                   EXSR      CHKNUM
043900121227     C                   IF        PiNum=*on
044000121227     C                   ADD(H)    PiVal         VABVLB
044100121227     C                   ELSE
044200121227     C                   ADD       1             errore
044300121227     C                   EVAL      vinmsg = %trimr(vinmsg)
044400121227     C                             + ' ' + 'VABVLB'
044500121227     C                   ENDIF
044600130315     C                   ENDIF
044700121227     C* ......VABIAS
044800121227     C                   IF        %subst(vindta:167:11) <> *blanks
044900121227     C                   EVAL      VABVAS=%trim(%subst(vindta:153:3))
045000121227     C                   EVAL      PiStr=%trim(%subst(vindta:167:11))
045100121227     C                   EXSR      CHKNUM
045200121227     C                   IF        PiNum=*on
045300121227     C                   Z-ADD     PiVal         VABIAS
045400121227     C                   ELSE
045500121227     C                   EVAL      vinmsg = %trimr(vinmsg)
045600121227     C                             + ' ' + 'VABIAS'
045700121227     C                   ENDIF
045800121227     C                   ENDIF
045900121227     C* ......VABCAS
046000121227     C                   IF        %subst(vindta:438:8) <> *blanks
046100121227     C                   EVAL      FlgCAS = '1'
046200121227     C                   EVAL      VABVCA=%trim(%subst(vindta:446:3))
046300121227     C                   EVAL      PiStr=%trim(%subst(vindta:438:8))
046400121227     C                   EXSR      CHKNUM
046500121227     C                   IF        PiNum=*on
046600121227     C                   Z-ADD     PiVal         VABCAS
046700121227     C                   ELSE
046800121227     C                   ADD       1             errore
046900121227     C                   EVAL      vinmsg = %trimr(vinmsg)
047000121227     C                             + ' ' + 'VABCAS'
047100121227     C                   ENDIF
047200121227     C                   ENDIF
047300121227     C* ......VABFFD
047400121227     C                   IF        %subst(vindta:474:4) = '0001'
047500121227     C                   EVAL      VABFFD = 'S'
047600121227     C                   ENDIF
047700121227     C*
047800121227     C                   ENDIF
047900121227     C*
048000121227     C********
048100121227     C* Tipo record 'C'
048200121227     C********
048300130315     C***                IF        %trim(%subst(vindta:1:1)) = 'C'
048400121227     C* ......VATNOT ("chi sono")
048500130315     C***                EVAL      VATNOT=%trim(%subst(vindta:95:70))
048600130315     C***                EXSR      exevate
048700121227     C*
048800130315     C***                ENDIF
048900130315     C********
049000130315     C* Tipo record 'D'
049100130315     C********
049200130315     C                   IF        %trim(%subst(vindta:1:1)) = 'D'
049300130315     C* ......VATNOT ("chi sono")
049400130315     C                   EVAL      VATNOT=%trim(%subst(vindta:41:13))
049500130315     C                   EXSR      exevate
049600130315     C*
049700130315     C                   ENDIF
049800121227     C*
049900121227     C* Considerazioni sul contenuto di campi precedentemente valorizzati
050000121227     C                   IF        FlgCAS <> '0'
050100121227     C                   IF        VABCBO = '1'
050200121227     C                   EVAL      VABCBO = '4'
050300121227     C                   ELSE
050400121227     C                   EVAL      VABCBO = '6'
050500121227     C                   ENDIF
050600121227     C                   ENDIF
050700121227     C*
050800121227     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050900121227     C                   EXSR      CHKIMPDIV
051000121227     C*
051100121227     C* Ebbene...
051200121227     C                   ADD       1             §CTRMO
051300121227     C                   IF        errore <> *zeros
051400121227     C                   ADD       1             §CTRNO
051500121227     C                   EVAL      vinflg = '2'
051600121227     C                   ELSE
051700121227     C                   ADD       1             §CTROKVB
051800121227     C                   ENDIF
051900121227     C*
052000121227     C                   ENDSR
052100070102     C*----------------------------------------------------*
052200070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
052300070102     C*----------------------------------------------------*
052400121227     C     EXEVATE       BEGSR
052500121227     C*
052600121227     C                   EVAL      VATTRC='E'
052700101007     C*
052800120802     C                   exsr      wrivat
052900070102     C*
053000070102     C                   ENDSR
053100080415     C*----------------------------------------------------*
053200121227     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
053300080415     C*----------------------------------------------------*
053400080415     C     EXEVATA       BEGSR
053500080415     C*
053600120802     C                   EVAL      VATTRC='A'
053700080415     C*
053800120802     C                   exsr      wrivat                                       => scarico VAT
053900080415     C*
054000080415     C                   ENDSR
054100071121     C*----------------------------------------------------*
054200121227     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
054300071121     C*----------------------------------------------------*
054400071121     C     EXEVATB       BEGSR
054500071121     C*
054600090213     C                   EVAL      VATTRC='B'
054700071121     C*
054800090213     C                   exsr      wrivat                                       => scarico VAT
054900071121     C*
055000071121     C                   ENDSR
055100010201     C*----------------------------------------------------*
055200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
055300010201     C*----------------------------------------------------*
055400020305     C     WRIVAT        BEGSR
055500050628     C*
055600060223     C* Scrivo solo se valorizzato qualcosa
055700060223     C                   IF        VATNOT <> *blanks
055800040802     C                   WRITE     FIVAT000
055900060223     C                   ENDIF
056000010201     C*
056100010201     C                   ENDSR
056200010202     C*----------------------------------------------------*
056300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056400010202     C*----------------------------------------------------*
056500020305     C     PREVAT        BEGSR
056600010202     C*
056700021113     C* Compongo il nome del membro da dare al FIVATWWR
056800010202     C                   eval      parmbr = vlrhdl
056900010202     C                   movel     'M'           parmbr
057000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
057100010202     C                   eval      paropz = '1'
057200010202     C* Effettuo la chiamata al CLLE preposto
057300040506     C                   call(e)   'TITVVTC'
057400010202     C                   parm                    parccm
057500010202     C                   parm                    parmbr
057600010202     C                   parm                    paropz
057700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057800010202     C                   if        %error
057900010202     C                   movel     '1'           chkcall
058000010202     C                   else
058100010202     C                   movel     '0'           chkcall
058200010202     C                   endif
058300010202     C*
058400010202     C                   ENDSR
058500000801     C*----------------------------------------------------*
058600000801     C*  CONTROLLO NUMERICITA' CAMPI
058700000801     C*----------------------------------------------------*
058800000801     C     CHKNUM        BEGSR
058900090113     C*
059000090113     C                   IF        PiDecChr = *blanks
059100121227     C                   EVAL      PiDecChr = '.'
059200090113     C                   ENDIF
059300090113     C*
059400090113     C                   callp(e)  UBISNUM_Check(PiStr
059500090113     C                                          :PiDecChr
059600090113     C                                          :PiVal
059700090113     C                                          :PiNum
059800090113     C                                          :PiInt)
059900090113     C*
060000090113     C                   IF        %error
060100090113     C                   EVAL      PiInt=*off
060200090113     C                   ENDIF
060300000801     C*
060400000801     C                   ENDSR
060500000801     C***
060600000801
060700011113
060800011113     C*----------------------------------------------------*
060900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061000011113     C*----------------------------------------------------*
061100011113     C     CHKIMPDIV     BEGSR
061200011113     C*
061300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061400011113     C                   Z-ADD     *zeros        wrkDec            9 9
061500011113     C*
061600011113     C* Come prima cosa effettuo considerazioni sulla divisa
061700011113     C                   IF        vabIAS > *zeros
061800011113     C                   IF        vabVAS <> 'EUR'
061900011113     C                   EVAL      vabVAS =  'ITL'
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C                   IF        vabCAS > *zeros
062400011113     C                   IF        vabVCA <> 'EUR'
062500011113     C                   EVAL      vabVCA =  'ITL'
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C                   IF        vabVMD > *zeros
063000020305     C                   IF        vabVAD <> 'EUR'
063100011113     C                   EVAL      vabVAD =  'ITL'
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063600011113     C                   Z-ADD     vabIAS        wrkDec
063700011113     C                   IF        wrkDec > *zeros
063800011113     C                   IF        vabVAS = 'ITL'
063900011113     C                   EVAL      vabIAS = *zeros
064000011113     C                   ENDIF
064100011113     C                   ENDIF
064200011113     C*
064300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064400011113     C                   Z-ADD     vabCAS        wrkDec
064500011113     C                   IF        wrkDec > *zeros
064600011113     C                   IF        vabVCA = 'ITL'
064700011113     C                   EVAL      vabCAS = *zeros
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065200011113     C                   Z-ADD     vabVMD        wrkDec
065300011113     C                   IF        wrkDec > *zeros
065400011113     C                   IF        vabVAD = 'ITL'
065500011113     C                   EVAL      vabVMD = *zeros
065600011113     C                   ENDIF
065700011113     C                   ENDIF
065800011113     C*
065900011113     C                   ENDSR
066000011113     C***
066100011113
066200011113
066300000801
066400000801
066500990920      /TITLE Invio dei dati al punto operativo.
066600010202     C     invio         BEGSR
066700990920     C*
066800021113     C* 1° invio FIVAT
066900010201     C                   reset                   dscmz
067000010201     C                   move      vlrpoi        cmzdst
067100021113     C                   eval      cmzfld = 'FIVATWWR'
067200010201     C                   eval      cmzmbd = vlrhdl
067300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067400021009     C***                if        prmfir = *blanks
067500021113     C                   eval      cmzfla = 'FIVAT00F'
067600021113     C                   eval      cmzmba = 'FIVAT00F'
067700021009     C***                else
067800021009     C***                eval      cmzfla = prmfir
067900021009     C***                eval      cmzmba = prmfir
068000021009     C***                endif
068100010201     C                   eval      cmznrr = *zeros
068200020305     C                   move      §ctrokvt      cmznrr
068300021018     C                   eval      cmzlba = vlrfl1
068400010201     C                   call(e)   'TIS711C'
068500010201     C                   parm                    dscmz
068600010201     C                   parm      *blanks       esito
068700010205     C                   if        %error
068800010205     C                             or cmzerr = '1'
068900010205     C                             or esito  = '1'
069000010205     C                   eval      wrkesito = '3'
069100010205     C                   else
069200010201     C*
069300021113     C* 2° invio FIVAB
069400010201     C                   reset                   dscmz
069500010201     C                   move      vlrpoi        cmzdst
069600010201     C                   eval      cmzfld = vlrfou
069700010201     C                   eval      cmzmbd = vlrhdl
069800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069900021009     C***                if        prmfir = *blanks
070000021113     C                   eval      cmzfla = 'FIVAB00F'
070100021113     C                   eval      cmzmba = 'FIVAB00F'
070200021009     C***                else
070300021009     C***                eval      cmzfla = prmfir
070400021009     C***                eval      cmzmba = prmfir
070500021009     C***                endif
070600010201     C                   eval      cmznrr = *zeros
070700010201     C                   move      §ctrokvb      cmznrr
070800021018     C                   eval      cmzlba = vlrfl1
070900010201     C                   call(e)   'TIS711C'
071000010201     C                   parm                    dscmz
071100010201     C                   parm      *blanks       esito
071200010201     C                   if        %error
071300010201     C                             or cmzerr = '1'
071400010201     C                             or esito  = '1'
071500010201     C                   eval      wrkesito = '3'
071600010201     C                   endif
071700010205     C                   endif
071800990920     C*
071900000613     C                   ENDSR
072000000613     C***
072100070411
072200070411     C     *pssr         BEGSR
072300070411     C*
072400070411     C                   if        %open(tivin00r)
072500070411     C                   close     tivin00r
072600070411     C                   endif
072700070411     C                   if        %open(fivabwwr)
072800070411     C                   close     fivabwwr
072900070411     C                   endif
073000070411     C                   if        %open(fivatwwr)
073100070411     C                   close     fivatwwr
073200070411     C                   endif
073300070411     C*
073400070411     C* Effettuo la chiamata al CLLE preposto
073500070411     C                   call(e)   'TITVVTC'
073600070411     C                   parm                    parccm
073700070411     C                   parm                    parmbr
073800070411     C                   parm      '2'           paropz
073900070411     C*
074000070411     C                   eval      wrkesito = '2'
074100070411     C*
074200070411     C                   seton                                        LR
074300070411     C*
074400070411     C                   ENDSR     '*CANCL'
074500070411     C***
074600121227
074700121227
074800121227
074900121227     C*--------------------------------------------------------
075000121227     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
075100121227     C*--------------------------------------------------------
075200121227     C     CARTAB        BEGSR
075300121227     C*
075400121227     C* TABELLA '15' - NAZIONI
075500121227     C                   clear                   skNAZISO
075600121227     C                   clear                   skNAZBAR
075700121227     C                   eval      tblKUT = 1
075800121227     C                   eval      tblCOD = '15'
075900121227     C     KEYtabP       setll     tabel00f
076000121227     C     KEYtabP       reade     tabel00f
076100121227     C                   dow       not %eof(tabel00f)
076200121227     C                   if        tblFLG = *blanks
076300121227     C                   movel(p)  tblUNI        ds15
076400121227     C                   add       1             jNAZ
076500121227     C                   eval      skNAZBAR(jNAZ) = tblKEY
076600121227     C                   eval      skNAZISO(jNAZ) = §15COD
076700121227     C                   endif
076800121227     C     KEYtabP       reade     tabel00f
076900121227     C                   enddo
077000121227     C*
077100121227     C                   ENDSR
077200121227     C***
077300070411
077400121227
077500990910
077600000613     C     *inzsr        BEGSR
077700990910     C*
077800990910     C     *entry        plist
077900990920     C                   parm                    tivlrds
078000990921     C                   parm      wrkesito      esito
078100000724     C                   parm                    prmlit
078200000710     C                   parm                    prmfir
078300000613     C*
078400000830     C* CALCOLA LA DATA CORRENTE
078500120813     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078600120813     C                   eval      datcor = %dec(%date() : *ISO)
078700121227     C*
078800121227     C* Chiave su TABEL00F - parziale
078900121227     C     KEYtabP       KLIST
079000121227     C                   KFLD                    tblKUT
079100121227     C                   KFLD                    tblCOD
079200000830     C*
079300000613     C                   ENDSR
079400000613     C***
