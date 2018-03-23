000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300120207     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
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
001600040506     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600120130     D wOut            s              1    INZ('0')
002700120207     D wNomeFile       s             30    INZ(*blanks)
002800000830
002900041025     D*------------------
003000041025     D* DS REPERIMENTO NUMERATORE
003100041025     D*------------------
003200041025     D trul33ds      e ds                  inz
003300041025     D*------------------
003400041025     D* DS ARCHITETTURA
003500041025     D*------------------
003600041025     D kpjba         e ds                  inz
003700060913     D*------------------
003800060913     D* DS RIDEFINIZIONE TIPO RECORD CLIENTE '25' - Informazioni aggiuntive
003900060913     D*------------------
004000060913     D dsRT25          ds                  inz
004100060928     D  dRT25_RT25                    2
004200060913     D  dRT25_TC1                     1
004300060913     D  dRT25_VATB                   25
004400071031     D  dRT25_VATA                   26
004500060913     D  dRT25_DCRDD                   2
004600060913     D  dRT25_DCRMM                   2
004700060913     D  dRT25_DCRYYYY                 4
004800060913     D*------------------
004900990908
005000081222
005100081222     D*------------------
005200081222     D* LINKING A DEFINIZIONI ESTERNE
005300081222     D*------------------
005400081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600010201
005700010201
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000613     C*
006200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006300000613     C*
006400010202     C* Effettuo la chiamata al CLLE preposto
006500040506     C                   call(e)   'TITVVTC'
006600010202     C                   parm                    parccm
006700010202     C                   parm                    parmbr
006800010202     C                   parm      '2'           paropz
006900040929     C*
007000040929     C* Esegue lancio TISI95R solo x chiusura
007100040929     C                   CLEAR                   TISI95DS
007200040929     C                   EVAL      I95TLA = 'C'
007300040929     C                   CALL      'TISI95R'
007400040929     C                   PARM                    TISI95DS
007500000616     C*
007600010201     C                   seton                                        LR
007700990908
007800000801
007900910830     C*--------------------------------------------------------
008000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008100910830     C*--------------------------------------------------------
008200040526     C     RWFILE        BEGSR
008300990910     C*
008400990914     C                   if        not %open(tivin00r)
008500990908     C                   open      tivin00r
008600990914     C                   endif
008700021113     C                   if        not %open(fivabwwr)
008800021113     C                   open      fivabwwr
008900990914     C                   endif
009000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009100020305     C                   exsr      prevat
009200010201     C*
009300010202     C                   if        chkcall = '0'
009400010202     C*
009500021113     C                   if        not %open(fivatwwr)
009600021113     C                   open      fivatwwr
009700010201     C                   endif
009800990910     C*
009900010201     C                   clear                   §CTROKVB          5 0
010000020305     C                   clear                   §CTROKVT          5 0
010100000801     C                   clear                   §CTRMO            5 0
010200000801     C                   clear                   §CTRNO            5 0
010300040910     C*
010400921023     C                   DO        *HIVAL
010500990913     C*
010600990915     C                   READ      tivin00r                               70
010700040910     C                   if        vindta > *blanks
010800000613     C                   add       1             rrnum
010900000801     C*
011000000801     C                   if        *in70 = *off
011100000801     C                             and
011200000801     C                             (vinflg = *blanks
011300000801     C                              or vinflg = '0'
011400000801     C                              or vinflg = '2')
011500000801     C*
011600000801     C                   clear                   vinmsg
011700000801     C                   eval      vinflg = '1'
011800040910     C*
011900040910     C* Eseguo routine d traduzione
012000040910     C                   exsr      impvabvat
012100040802     C*
012200010305     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100010202     C*
013200120130     C* Scarico il buffer d output in sospeso ...
013300120130     C                   WRITE     FIVAB000
013400120130     C*
013500010202     C                   endif
013600990910
013700990910     C* Se non ci sono record con errori ...
013800000710     C                   if        §ctrno = 0
013900990910     C* ... restituisco esito OK.
014000990921     C                   eval      wrkesito = '0'
014100990910     C                   else
014200010201     C                   if        §ctrokvb > 0
014300990921     C                   eval      wrkesito = '1'
014400000710     C                   else
014500000710     C                   eval      wrkesito = '2'
014600990910     C                   endif
014700000710     C                   endif
014800990910     C*
014900990914     C                   if        %open(tivin00r)
015000990908     C                   close     tivin00r
015100990914     C                   endif
015200021113     C                   if        %open(fivabwwr)
015300021113     C                   close     fivabwwr
015400990914     C                   endif
015500021113     C                   if        %open(fivatwwr)
015600021113     C                   close     fivatwwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   if        §ctrokvb > 0
016000000724     C                             and vlrpoi <> *zeros
016100010202     C                   exsr      invio
016200990920     C                   endif
016300990920     C*
016400910830     C                   ENDSR
016500000613     C***
016600990920
016700000801     C*----------------------------------------------------*
016800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016900000801     C*----------------------------------------------------*
017000010201     C     INZVAR        BEGSR
017100000801     C*
017200040802     C                   Z-ADD     *zeros        Num5_0            5 0
017300040802     C                   MOVEL     '0'           FlgCAS            1
017400000801     C*
017500000801     C                   ENDSR
017600000801     C*----------------------------------------------------*
017700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017800000801     C*----------------------------------------------------*
017900000801     C     DEFCAM        BEGSR
018000000801     C*
018100021113     C                   CLEAR                   FIVAB000
018200040802     C                   CLEAR                   FIVAT000
018300020619     C* Imposto i valori di default...
018400141024     C                   Z-ADD     0436614       VABCCM
018500141024     C                   Z-ADD     0436614       VATCCM
018600141024     C                   Z-ADD     043           VABLNP
018700141024     C                   Z-ADD     043           VATLNP
018800141024     C                   Z-ADD     000           VABCTR
018900040714     C                   MOVEL     '7Q'          VABCTM
019000040823     C                   MOVEL     '1'           VABCBO
019100020619     C* ... e poi verifico se sono stati passati come parametri
019200020619     C                   IF        vlrppt > *blanks
019300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019500020619     C                   EXSR      CHKNUM
019600020619     C                   IF        PiInt=*on
019700020619     C                   Z-ADD     PiVal         VABCCM
019800020619     C                   Z-ADD     PiVal         VATCCM
019900020619     C                   ENDIF
020000040506     C                   ENDIF
020100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020300020619     C                   EXSR      CHKNUM
020400020619     C                   IF        PiInt=*on
020500020619     C                   Z-ADD     PiVal         VABLNP
020600020619     C                   Z-ADD     PiVal         VATLNP
020700040506     C                   ENDIF
020800020619     C                   ENDIF
020900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021100020619     C                   EXSR      CHKNUM
021200020619     C                   IF        PiInt=*on
021300020619     C                   Z-ADD     PiVal         VABCTR
021400040506     C                   ENDIF
021500020619     C                   ENDIF
021600070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
021700070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
021800070427     C                   ENDIF
021900020619     C                   ENDIF
022000000801     C*
022100000801     C                   ENDSR
022200000801     C*----------------------------------------------------*
022300040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
022400000801     C*----------------------------------------------------*
022500040910     C     IMPVABVAT     BEGSR
022600040910     C*
022700040910     C* Traduzione relativa ai tipi record del file del cliente
022800040910     C*
022900040910     C***
023000040910     C* ...tipo record 11
023100040910     C                   IF        %subst(vindta:1:2) = '11'
023200120130     C*
023300120130     C* Se buffer da scaricare => scarico ...
023400120130     C                   IF        wOut = '1'
023500120130     C                   WRITE     FIVAB000
023600120130     C                   ENDIF
023700120130     C*
023800120130     C                   EVAL      wOut = '1'
023900120130     C*
024000040910     C* ......inizializzazioni iniziali e formati record file Bartolini
024100040910     C                   EXSR      INZVAR
024200040910     C                   EXSR      DEFCAM
024300081222     C*
024400081222     C                   Z-ADD     *zeros        errore            1 0
024500081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
024600081222     C                   MOVEL     datcor        VABAAS
024700081222     C                   MOVEL     datcor        VATAAS
024800081222     C                   MOVE      datcor        VABMGS
024900081222     C                   MOVE(P)   vlrpoi        VABFGS
025000081222     C                   MOVE(P)   vlrpoi        VATFGS
025100071011     C*
025200120130     C****               IF        %subst(vindta:3:9) = '934397260'             * MILLIPORE
025300120130     C****               EVAL      VABCCM = 3110003
025400120130     C****               EVAL      VATCCM = 3110003
025500120130     C****               EVAL      VABFGS = 311
025600120130     C****               EVAL      VATFGS = 311
025700120130     C****               ENDIF
025800120130     C*
025900040910     C* ......VABNSP/VATNSP
026000041025     C*                  EVAL      PiStr=%trim(%subst(vindta:21:6))
026100041025     C*                  EXSR      CHKNUM
026200041025     C*                  IF        PiInt=*on
026300041025     C*                  Z-ADD     PiVal         VABNSP
026400041025     C*                  Z-ADD     PiVal         VATNSP
026500041025     C*                  ELSE
026600041025     C*                  ADD       1             errore
026700041025     C*                  EVAL      vinmsg = %trimr(vinmsg)
026800041025     C*                            + ' ' + 'VABNSP/VATNSP'
026900041025     C*                  ENDIF
027000041025     C* NSP => Stacco un numeratore da AZNUM
027100041025     C                   clear                   TRUL33DS
027200041025     C                   eval      I33OPE = *zeros
027300041025     C                   eval      I33CNU = 302
027400041025     C                   eval      I33NUM = 1
027500041025     C                   movel     TRUL33DS      KPJBU
027600041025     C                   call      'TRUL33R'
027700041025     C                   parm                    KPJBA
027800041025     C                   movel     KPJBU         TRUL33DS
027900041025     C                   if        O33ERR = *zeros
028000041025     C                   z-add     O33NRF        VABNSP
028100041025     C                   z-add     O33NRF        VATNSP
028200041025     C                   else
028300041025     C                   Z-ADD     1             errore
028400041025     C                   EVAL      vinmsg = %trimr(vinmsg)
028500041025     C                             + ' ' + 'VABNSP VATNSP'
028600041025     C                   endif
028700040910     C* ......VABRSD
028800040910     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
028900060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
029000060928     C     '@':'A'       XLATE     VABRSD        VABRSD
029100060928     C     '§':'S'       XLATE     VABRSD        VABRSD
029200060928     C     '!':' '       XLATE     VABRSD        VABRSD
029300060928     C     ':':' '       XLATE     VABRSD        VABRSD
029400060928     C*
029500040910     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
029600060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
029700060928     C     '@':'A'       XLATE     VABRD2        VABRD2
029800060928     C     '§':'S'       XLATE     VABRD2        VABRD2
029900060928     C     '!':' '       XLATE     VABRD2        VABRD2
030000060928     C     ':':' '       XLATE     VABRD2        VABRD2
030100040910     C*
030200040910     C                   ENDIF
030300040910     C***
030400040910     C* ...tipo record 12
030500040910     C                   IF        %subst(vindta:1:2) = '12'
030600040910     C* ......VABIND
030700040910     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
030800141024     C                   IF        VABIND = *blanks AND VABRD2 <> *blanks
030900141024     C                   EVAL      VABIND = VABRD2
031000141024     C                   EVAL      VABRD2 = *blanks
031100141024     C                   ENDIF
031200040910     C* ......VABCAD
031300040910     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
031400040910     C                   EXSR      CHKNUM
031500040910     C                   IF        PiInt=*on
031600040910     C                   Z-ADD     PiVal         Num5_0
031700040910     C                   MOVEL(P)  Num5_0        VABCAD
031800040910     C                   ELSE
031900040910     C                   ADD       1             errore
032000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
032100040910     C                             + ' ' + 'VABCAD'
032200040910     C                   ENDIF
032300040910     C* ......VABLOD
032400040910     C                   EVAL      VABLOD=%trim(%subst(vindta:100:25))
032500040910     C* Reperisco la provincia dal CAP e dalla località
032600040910     C                   IF        VABCAD <> *blanks AND
032700040910     C                             VABPRD  = *blanks
032800040910     C                   CLEAR                   TISI95DS
032900040910     C                   EVAL      I95TCN = '3'
033000040910     C                   Z-ADD     datcor        I95DAT
033100040910     C                   EVAL      I95CAP = VABCAD
033200040910     C                   EVAL      I95LOC = VABLOD
033300040910     C                   CALL      'TISI95R'
033400040910     C                   PARM                    TISI95DS
033500040910     C                   EVAL      VABPRD = O95PRV
033600040910     C                   ENDIF
033700141126     C* ......VABFFFD
033800141126     C                   IF        %subst(vindta:125:2) = 'SX'
033900141126     C                   EVAL      VABFFD = 'S'
034000141126     C                   ENDIF
034100040910     C*
034200040910     C                   ENDIF
034300040910     C***
034400040910     C* ...tipo record 13
034500040910     C                   IF        %subst(vindta:1:2) = '13'
034600050719     C                   Z-ADD     *zeros        VABNCL
034700081222     C* ......VABRMA
034800081222     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
034900120130     C* ......VABRMN
035000141024     C                   EVAL      PiStr=%trim(%subst(vindta:82:6))
035100120130     C                   EXSR      CHKNUM
035200120130     C                   IF        PiInt=*on
035300120130     C                   ADD       PiVal         VABRMN
035400120130     C                   ELSE
035500120130     C                   ADD       1             errore
035600120130     C                   EVAL      vinmsg = %trimr(vinmsg)
035700120130     C                             + ' ' + 'VABRMN'
035800120130     C                   ENDIF
035900040910     C* ......VABNCL
036000141024     C                   SELECT
036100141024     C                   WHEN      %trim(%subst(vindta:31:3))<>*all'0'          * colli
036200141024     C                   EVAL      VABCTR = 000
036300141024     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
036400141024     C*
036500141024     C                   WHEN      %trim(%subst(vindta:27:2))<>*all'0'          * bancali
036600141024     C                   EVAL      VABCTR = 100
036700141024     C                   EVAL      PiStr=%trim(%subst(vindta:27:2))
036800141024     C                   ENDSL
036900141024     C*
037000040910     C                   EXSR      CHKNUM
037100040910     C                   IF        PiInt=*on
037200050719     C                   ADD       PiVal         VABNCL
037300040910     C                   ELSE
037400040910     C                   ADD       1             errore
037500040910     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040910     C                             + ' ' + 'VABNCL'
037700040910     C                   ENDIF
037800040910     C* ......VABPKB
037900040910     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
038000040910     C                   EXSR      CHKNUM
038100040910     C                   IF        PiNum=*on
038200040910     C                   EVAL(H)   VABPKB=PiVal/100                             * gestisco 2 dec.
038300040910     C                   ELSE
038400040910     C                   ADD       1             errore
038500040910     C                   EVAL      vinmsg = %trimr(vinmsg)
038600040910     C                             + ' ' + 'VABPKB'
038700040910     C                   ENDIF
038800040929     C* ......VABCBO
038900090701     C                   IF        %subst(vindta:46:1) = '4'
039000040929     C                   EVAL      VABCBO='2'
039100040929     C                   ELSE
039200040929     C                   EVAL      VABCBO='1'
039300040929     C                   ENDIF
039400040910     C* ......VABDCR
039500040910     C                   IF        %subst(vindta:47:6) <> *zeros
039600040910     C                   EVAL      PiStr='20'+%trim(%subst(vindta:47:6))
039700040910     C                   EXSR      CHKNUM
039800040910     C                   IF        PiInt=*on
039900040910     C                   Z-ADD     PiVal         VABDCR
040000040910     C                   ELSE
040100040910     C                   ADD       1             errore
040200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
040300040910     C                             + ' ' + 'VABDCR'
040400040910     C                   ENDIF
040500040910     C                   ENDIF
040600040910     C* ......VABHCR
040700040910     C                   EVAL      PiStr=%trim(%subst(vindta:53:4))
040800040910     C                   EXSR      CHKNUM
040900040910     C                   IF        PiInt=*on
041000040910     C                   Z-ADD     PiVal         VABHCR
041100040910     C                   ELSE
041200040910     C                   ADD       1             errore
041300040910     C                   EVAL      vinmsg = %trimr(vinmsg)
041400040910     C                             + ' ' + 'VABHCR'
041500040910     C                   ENDIF
041600040910     C* ......VABCAS
041700040910     C                   IF        %subst(vindta:57:9) <> *zeros
041800040910     C                   EVAL      FlgCAS = '1'
041900040910     C                   ENDIF
042000040910     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
042100040910     C                   EXSR      CHKNUM
042200040910     C                   IF        PiNum=*on
042300040910     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
042400040910     C                   ELSE
042500040910     C                   ADD       1             errore
042600040910     C                   EVAL      vinmsg = %trimr(vinmsg)
042700040910     C                             + ' ' + 'VABCAS'
042800040910     C                   ENDIF
042900110621     C* ......VABTIC
043000110621     C                   SELECT
043100110621     C                   WHEN      %subst(vindta:66:1) = '3'
043200110621     C                   EVAL      VABTIC=*blanks
043300110621     C                   WHEN      %subst(vindta:66:1) = '5'
043400110621     C                   EVAL      VABTIC='OM'
043500110621     C                   ENDSL
043600040910     C* ......VABVCA
043700040910     C                   EVAL      VABVCA=%trim(%subst(vindta:67:3))
043800040910     C* ......VABIAS
043900060126     C*                  EVAL      PiStr=%trim(%subst(vindta:106:9))
044000060126     C*                  EXSR      CHKNUM
044100060126     C*                  IF        PiNum=*on
044200060126     C*                  EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
044300060126     C*                  ELSE
044400060126     C*                  ADD       1             errore
044500060126     C*                  EVAL      vinmsg = %trimr(vinmsg)
044600060126     C*                            + ' ' + 'VABIAS'
044700060126     C*                  ENDIF
044800040910     C* ......VABVAS
044900040910     C                   EVAL      VABVAS=%trim(%subst(vindta:115:3))
045000040910     C*
045100040910     C                   ENDIF
045200040910     C***
045300040910     C* ...tipo record 14
045400040910     C                   IF        %subst(vindta:1:2) = '14'
045500081222     C* ......VATNOT - "E"
045600040910     C                   EVAL      VATNOT=%trim(%subst(vindta:29:24))
045700040910     C                   EVAL      VATTRC = 'E'
045800040910     C* x ogni tipo record 14 devo scaricare il buffer del file FIVAT
045900040910     C                   WRITE     FIVAT000
046000040910     C*
046100040910     C                   ENDIF
046200041111     C***
046300041111     C* ...tipo record 22
046400041111     C                   IF        %subst(vindta:1:2) = '22'
046500041111     C* ......VABRMO
046600041111     C                   EVAL      VABRMO=%trim(%subst(vindta:3:30))
046700041111     C                   ENDIF
046800060913     C***
046900060914     C* ...tipo record 25
047000071031     C                   IF        %subst(vindta:1:2) = '25'
047100071031     C*
047200071031     C                   EVAL      dsRT25 = vindta
047300071119     C* Consegna particolare (solo valori ben determinati)
047400071119     C                   IF        dRT25_TC1 = 'A' or
047500071119     C                             dRT25_TC1 = 'P' or
047600071119     C                             dRT25_TC1 = 'S'
047700071031     C                   EVAL      VABTC1 = dRT25_TC1
047800071119     C                   ENDIF
047900060913     C* Telefono destinatario
048000071031     C                   EVAL      VATNOT = dRT25_VATB
048100071031     C                   EVAL      VATTRC = 'B'
048200071031     C                   WRITE     FIVAT000
048300061013     C* Referente consegna
048400071031     C                   EVAL      VATNOT = dRT25_VATA
048500071031     C                   EVAL      VATTRC = 'A'
048600071031     C                   WRITE     FIVAT000
048700060913     C* Data consegna richiesta
048800071219     C                   EVAL      PiStr=dRT25_DCRYYYY+dRT25_DCRMM+dRT25_DCRDD
048900071219     C                   EXSR      CHKNUM
049000071219     C                   IF        PiInt=*on
049100071219     C                   Z-ADD     PiVal         VABDCR
049200071219     C                   ELSE
049300071219     C                   Z-ADD     *zeros        VABDCR
049400071219     C                   ADD       1             errore
049500071219     C                   EVAL      vinmsg = %trimr(vinmsg)
049600071219     C                             + ' ' + 'VABDCR'
049700071219     C                   ENDIF
049800071031     C*
049900071031     C                   ENDIF
050000040910     C***
050100040910     C* ...tipo record 41
050200040910     C                   IF        %subst(vindta:1:2) = '41'
050300040910     C* ......VABNAS
050400040910     C                   EVAL      VABNAS=%trim(%subst(vindta:3:126))
050500040910     C*
050600040910     C                   ENDIF
050700040910     C***
050800040910     C* ...tipo record 42
050900040910     C                   IF        %subst(vindta:1:2) = '42'
051000040910     C* ......VABNOT/VABNT2
051100040910     C                   EVAL      VABNOT=%trim(%subst(vindta:3:35))
051200040910     C                   EVAL      VABNT2=%trim(%subst(vindta:3+35:35))
051300040910     C*
051400040910     C                   ENDIF
051500040910     C***
051600010202     C*
051700000801     C* Ebbene...
051800000801     C                   ADD       1             §CTRMO
051900010201     C                   IF        errore <> *zeros
052000000801     C                   ADD       1             §CTRNO
052100000801     C                   EVAL      vinflg = '2'
052200000801     C                   ELSE
052300010201     C                   ADD       1             §CTROKVB
052400000801     C                   ENDIF
052500000801     C*
052600000801     C                   ENDSR
052700010202     C*----------------------------------------------------*
052800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052900010202     C*----------------------------------------------------*
053000020305     C     PREVAT        BEGSR
053100010202     C*
053200021113     C* Compongo il nome del membro da dare al FIVATWWR
053300010202     C                   eval      parmbr = vlrhdl
053400010202     C                   movel     'M'           parmbr
053500060113     C                   eval      parccm = vlrksc
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
055400081222     C*
055500081222     C                   IF        PiDecChr = *blanks
055600081222     C                   EVAL      PiDecChr = ','
055700081222     C                   ENDIF
055800081222     C*
055900081222     C                   callp(e)  UBISNUM_Check(PiStr
056000081222     C                                          :PiDecChr
056100081222     C                                          :PiVal
056200081222     C                                          :PiNum
056300081222     C                                          :PiInt)
056400081222     C*
056500081222     C                   IF        %error
056600081222     C                   EVAL      PiInt=*off
056700081222     C                   ENDIF
056800000801     C*
056900000801     C                   ENDSR
057000000801     C***
057100000801
057200011113
057300011113     C*----------------------------------------------------*
057400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057500011113     C*----------------------------------------------------*
057600011113     C     CHKIMPDIV     BEGSR
057700011113     C*
057800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057900011113     C                   Z-ADD     *zeros        wrkDec            9 9
058000011113     C*
058100011113     C* Come prima cosa effettuo considerazioni sulla divisa
058200011113     C                   IF        vabIAS > *zeros
058300011113     C                   IF        vabVAS <> 'EUR'
058400011113     C                   EVAL      vabVAS =  'ITL'
058500011113     C                   ENDIF
058600011113     C                   ENDIF
058700011113     C*
058800011113     C                   IF        vabCAS > *zeros
058900011113     C                   IF        vabVCA <> 'EUR'
059000011113     C                   EVAL      vabVCA =  'ITL'
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C                   IF        vabVMD > *zeros
059500020305     C                   IF        vabVAD <> 'EUR'
059600011113     C                   EVAL      vabVAD =  'ITL'
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060100011113     C                   Z-ADD     vabIAS        wrkDec
060200011113     C                   IF        wrkDec > *zeros
060300011113     C                   IF        vabVAS = 'ITL'
060400011113     C                   EVAL      vabIAS = *zeros
060500011113     C                   ENDIF
060600011113     C                   ENDIF
060700011113     C*
060800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060900011113     C                   Z-ADD     vabCAS        wrkDec
061000011113     C                   IF        wrkDec > *zeros
061100011113     C                   IF        vabVCA = 'ITL'
061200011113     C                   EVAL      vabCAS = *zeros
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061700011113     C                   Z-ADD     vabVMD        wrkDec
061800011113     C                   IF        wrkDec > *zeros
061900011113     C                   IF        vabVAD = 'ITL'
062000011113     C                   EVAL      vabVMD = *zeros
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C                   ENDSR
062500011113     C***
062600011113
062700011113
062800000801
062900000801
063000990920      /TITLE Invio dei dati al punto operativo.
063100010202     C     invio         BEGSR
063200990920     C*
063300021113     C* 1° invio FIVAT
063400010201     C                   reset                   dscmz
063500010201     C                   move      vlrpoi        cmzdst
063600021113     C                   eval      cmzfld = 'FIVATWWR'
063700010201     C                   eval      cmzmbd = vlrhdl
063800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063900021009     C***                if        prmfir = *blanks
064000021113     C                   eval      cmzfla = 'FIVAT00F'
064100021113     C                   eval      cmzmba = 'FIVAT00F'
064200021009     C***                else
064300021009     C***                eval      cmzfla = prmfir
064400021009     C***                eval      cmzmba = prmfir
064500021009     C***                endif
064600010201     C                   eval      cmznrr = *zeros
064700020305     C                   move      §ctrokvt      cmznrr
064800021018     C                   eval      cmzlba = vlrfl1
064900010201     C                   call(e)   'TIS711C'
065000010201     C                   parm                    dscmz
065100010201     C                   parm      *blanks       esito
065200010205     C                   if        %error
065300010205     C                             or cmzerr = '1'
065400010205     C                             or esito  = '1'
065500010205     C                   eval      wrkesito = '3'
065600010205     C                   else
065700010201     C*
065800021113     C* 2° invio FIVAB
065900010201     C                   reset                   dscmz
066000010201     C                   move      vlrpoi        cmzdst
066100010201     C                   eval      cmzfld = vlrfou
066200010201     C                   eval      cmzmbd = vlrhdl
066300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066400021009     C***                if        prmfir = *blanks
066500021113     C                   eval      cmzfla = 'FIVAB00F'
066600021113     C                   eval      cmzmba = 'FIVAB00F'
066700021009     C***                else
066800021009     C***                eval      cmzfla = prmfir
066900021009     C***                eval      cmzmba = prmfir
067000021009     C***                endif
067100010201     C                   eval      cmznrr = *zeros
067200010201     C                   move      §ctrokvb      cmznrr
067300021018     C                   eval      cmzlba = vlrfl1
067400010201     C                   call(e)   'TIS711C'
067500010201     C                   parm                    dscmz
067600010201     C                   parm      *blanks       esito
067700010201     C                   if        %error
067800010201     C                             or cmzerr = '1'
067900010201     C                             or esito  = '1'
068000010201     C                   eval      wrkesito = '3'
068100010201     C                   endif
068200010205     C                   endif
068300990920     C*
068400000613     C                   ENDSR
068500000613     C***
068600070411
068700071219     C     *pssr         BEGSR
068800070411     C*
068900071219     C                   if        %open(tivin00r)
069000071219     C                   close     tivin00r
069100071219     C                   endif
069200071219     C                   if        %open(fivabwwr)
069300071219     C                   close     fivabwwr
069400071219     C                   endif
069500071219     C                   if        %open(fivatwwr)
069600071219     C                   close     fivatwwr
069700071219     C                   endif
069800070411     C*
069900070411     C* Effettuo la chiamata al CLLE preposto
070000071219     C                   call(e)   'TITVVTC'
070100071219     C                   parm                    parccm
070200071219     C                   parm                    parmbr
070300071219     C                   parm      '2'           paropz
070400071219     C*
070500071219     C                   eval      wrkesito = '2'
070600071219     C*
070700071219     C                   seton                                        LR
070800070411     C*
070900071219     C                   ENDSR     '*CANCL'
071000070411     C***
071100120207
071200070411
071300990910
071400000613     C     *inzsr        BEGSR
071500990910     C*
071600990910     C     *entry        plist
071700990920     C                   parm                    tivlrds
071800990921     C                   parm      wrkesito      esito
071900000724     C                   parm                    prmlit
072000000710     C                   parm                    prmfir
072100120207     C*
072200120207     C                   MOVE(P)   vlrMSG        wNomeFile
072300000613     C*
072400000830     C* CALCOLA LA DATA CORRENTE
072500120207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072600120207     C                   eval      datcor = %dec(%date() : *iso)
072700000830     C*
072800000613     C                   ENDSR
072900000613     C***
