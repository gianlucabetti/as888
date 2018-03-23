000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081222     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
001500040506     D tisi95ds      e ds
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
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400041025     D*------------------
003500041025     D* DS REPERIMENTO NUMERATORE
003600041025     D*------------------
003700041025     D trul33ds      e ds                  inz
003800041025     D*------------------
003900041025     D* DS ARCHITETTURA
004000041025     D*------------------
004100041025     D kpjba         e ds                  inz
004200060913     D*------------------
004300060913     D* DS RIDEFINIZIONE TIPO RECORD CLIENTE '25' - Informazioni aggiuntive
004400060913     D*------------------
004500060913     D dsRT25          ds                  inz
004600060928     D  dRT25_RT25                    2
004700060913     D  dRT25_TC1                     1
004800060913     D  dRT25_VATB                   25
004900071031     D  dRT25_VATA                   26
005000060913     D  dRT25_DCRDD                   2
005100060913     D  dRT25_DCRMM                   2
005200060913     D  dRT25_DCRYYYY                 4
005300060913     D*------------------
005400990908
005500081222
005600081222     D*------------------
005700081222     D* LINKING A DEFINIZIONI ESTERNE
005800081222     D*------------------
005900081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006000081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006100010201
006200010201
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600000613     C*
006700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006800000613     C*
006900010202     C* Effettuo la chiamata al CLLE preposto
007000040506     C                   call(e)   'TITVVTC'
007100010202     C                   parm                    parccm
007200010202     C                   parm                    parmbr
007300010202     C                   parm      '2'           paropz
007400040929     C*
007500040929     C* Esegue lancio TISI95R solo x chiusura
007600040929     C                   CLEAR                   TISI95DS
007700040929     C                   EVAL      I95TLA = 'C'
007800040929     C                   CALL      'TISI95R'
007900040929     C                   PARM                    TISI95DS
008000000616     C*
008100120306     C                   seton                                        LR
008200990908
008300000801
008400910830     C*--------------------------------------------------------
008500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008600910830     C*--------------------------------------------------------
008700040526     C     RWFILE        BEGSR
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
010800040910     C*
010900921023     C                   DO        *HIVAL
011000990913     C*
011100990915     C                   READ      tivin00r                               70
011200040910     C                   if        vindta > *blanks
011300000613     C                   add       1             rrnum
011400000801     C*
011500000801     C                   if        *in70 = *off
011600000801     C                             and
011700000801     C                             (vinflg = *blanks
011800000801     C                              or vinflg = '0'
011900000801     C                              or vinflg = '2')
012000000801     C*
012100000801     C                   clear                   vinmsg
012200000801     C                   eval      vinflg = '1'
012300040910     C*
012400040910     C* Eseguo routine d traduzione
012500040910     C                   exsr      impvabvat
012600040802     C*
012700010305     C                   endif
012800000905     C*
012900000905     C                   else
013000000905     C                   eval      vinflg = '1'
013100000905     C                   endif
013200000905     C*
013300000905     C  N70              update    tivin000
013400000905     C*
013500991022     C  N70              ENDdo
013600010202     C*
013700010202     C                   endif
013800990910
013900990910     C* Se non ci sono record con errori ...
014000000710     C                   if        §ctrno = 0
014100990910     C* ... restituisco esito OK.
014200990921     C                   eval      wrkesito = '0'
014300990910     C                   else
014400010201     C                   if        §ctrokvb > 0
014500990921     C                   eval      wrkesito = '1'
014600000710     C                   else
014700000710     C                   eval      wrkesito = '2'
014800990910     C                   endif
014900000710     C                   endif
015000990910     C*
015100990914     C                   if        %open(tivin00r)
015200990908     C                   close     tivin00r
015300990914     C                   endif
015400021113     C                   if        %open(fivabwwr)
015500021113     C                   close     fivabwwr
015600990914     C                   endif
015700021113     C                   if        %open(fivatwwr)
015800021113     C                   close     fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   if        §ctrokvb > 0
016200000724     C                             and vlrpoi <> *zeros
016300010202     C                   exsr      invio
016400990920     C                   endif
016500990920     C*
016600910830     C                   ENDSR
016700000613     C***
016800990920
016900000801     C*----------------------------------------------------*
017000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017100000801     C*----------------------------------------------------*
017200010201     C     INZVAR        BEGSR
017300000801     C*
017400040802     C                   Z-ADD     *zeros        Num5_0            5 0
017500040802     C                   MOVEL     '0'           FlgCAS            1
017600000801     C*
017700000801     C                   ENDSR
017800000801     C*----------------------------------------------------*
017900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
018000000801     C*----------------------------------------------------*
018100000801     C     DEFCAM        BEGSR
018200000801     C*
018300021113     C                   CLEAR                   FIVAB000
018400040802     C                   CLEAR                   FIVAT000
018500020619     C* Imposto i valori di default...
018600040930     C                   Z-ADD     0892930       VABCCM
018700040930     C                   Z-ADD     0892930       VATCCM
018800040910     C                   Z-ADD     089           VABLNP
018900040910     C                   Z-ADD     089           VATLNP
019000040714     C                   Z-ADD     000           VABCTR
019100040714     C                   MOVEL     '7Q'          VABCTM
019200040823     C                   MOVEL     '1'           VABCBO
019300060206     C                   MOVEL     'D'           VABTSP
019400020619     C* ... e poi verifico se sono stati passati come parametri
019500020619     C                   IF        vlrppt > *blanks
019600081222     C* => Cliente (fino al 31/12/2008)
019700081222     C                   IF        datcor <= 20081231
019800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020000020619     C                   EXSR      CHKNUM
020100020619     C                   IF        PiInt=*on
020200020619     C                   Z-ADD     PiVal         VABCCM
020300020619     C                   Z-ADD     PiVal         VATCCM
020400020619     C                   ENDIF
020500040506     C                   ENDIF
020600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020800020619     C                   EXSR      CHKNUM
020900020619     C                   IF        PiInt=*on
021000020619     C                   Z-ADD     PiVal         VABLNP
021100020619     C                   Z-ADD     PiVal         VATLNP
021200040506     C                   ENDIF
021300020619     C                   ENDIF
021400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCTR
021900040506     C                   ENDIF
022000020619     C                   ENDIF
022100070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
022200070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
022300070427     C                   ENDIF
022400081222     C* => Partner (dal 01/01/2009)
022500081222     C                   ELSE
022600081222     C                   IF        %subst(vlrppt:1+14:7) <> *blanks
022700081222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1+14:7))
022800081222     C                   EXSR      CHKNUM
022900081222     C                   IF        PiInt=*on
023000081222     C                   Z-ADD     PiVal         VABCCM
023100081222     C                   Z-ADD     PiVal         VATCCM
023200081222     C                   ENDIF
023300081222     C                   ENDIF
023400081222     C                   IF        %subst(vlrppt:8+14:3) <> *blanks
023500081222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8+14:3))
023600081222     C                   EXSR      CHKNUM
023700081222     C                   IF        PiInt=*on
023800081222     C                   Z-ADD     PiVal         VABLNP
023900081222     C                   Z-ADD     PiVal         VATLNP
024000081222     C                   ENDIF
024100081222     C                   ENDIF
024200081222     C                   IF        %subst(vlrppt:11+14:3) <> *blanks
024300081222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11+14:3))
024400081222     C                   EXSR      CHKNUM
024500081222     C                   IF        PiInt=*on
024600081222     C                   Z-ADD     PiVal         VABCTR
024700081222     C                   ENDIF
024800081222     C                   ENDIF
024900081222     C                   IF        %subst(vlrppt:14+14:1) <> *blanks
025000081222     C                   EVAL      VABTSP = %subst(vlrppt:14+14:1)
025100081222     C                   ENDIF
025200081222     C                   ENDIF
025300020619     C                   ENDIF
025400000801     C*
025500000801     C                   ENDSR
025600000801     C*----------------------------------------------------*
025700040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
025800000801     C*----------------------------------------------------*
025900040910     C     IMPVABVAT     BEGSR
026000040910     C*
026100040910     C* Traduzione relativa ai tipi record del file del cliente
026200040910     C*
026300040910     C***
026400040910     C* ...tipo record 11
026500040910     C                   IF        %subst(vindta:1:2) = '11'
026600040910     C* ......inizializzazioni iniziali e formati record file Bartolini
026700040910     C                   EXSR      INZVAR
026800040910     C                   EXSR      DEFCAM
026900081222     C*
027000081222     C                   Z-ADD     *zeros        errore            1 0
027100081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
027200081222     C                   MOVEL     datcor        VABAAS
027300081222     C                   MOVEL     datcor        VATAAS
027400081222     C                   MOVE      datcor        VABMGS
027500081222     C                   MOVE(P)   vlrpoi        VABFGS
027600081222     C                   MOVE(P)   vlrpoi        VATFGS
027700071011     C*
027800071011     C* verifico forzatura x CCM
027900081222     C* => Cliente (fino al 31/12/2008)
028000081222     C                   IF        datcor <= 20081231
028100071018     C                   IF        %subst(vindta:3:9) = '934397260'             * MILLIPORE
028200071011     C                   EVAL      VABCCM = 1161200
028300071011     C                   EVAL      VATCCM = 1161200
028400071011     C                   ENDIF
028500080206     C***                IF        %subst(vindta:3:9) = '935395440'             * STAEDLER
028600080206     C                   IF        %subst(vindta:3:9) = '935396200'             * STAEDLER
028700071011     C                   EVAL      VABCCM = 1161201
028800071011     C                   EVAL      VATCCM = 1161201
028900071011     C                   ENDIF
029000081222     C* => Partner (dal 01/01/2009)
029100081222     C                   ELSE
029200081222     C                   IF        %subst(vindta:3:9) = '934397260'             * MILLIPORE
029300081222     C                   EVAL      VABCCM = 3110003
029400081222     C                   EVAL      VATCCM = 3110003
029500081222     C                   EVAL      VABFGS = 311
029600081222     C                   EVAL      VATFGS = 311
029700081222     C                   ENDIF
029800081222     C                   IF        %subst(vindta:3:9) = '935396200'             * STAEDLER
029900081222     C                   EVAL      VABCCM = 3110002
030000081222     C                   EVAL      VATCCM = 3110002
030100081222     C                   EVAL      VABFGS = 311
030200081222     C                   EVAL      VATFGS = 311
030300081222     C                   ENDIF
030400100712     C                   IF        %subst(vindta:3:9) = '932197780'             * PAULMANN
030500100712     C                   EVAL      VABCCM = 3110010
030600100712     C                   EVAL      VATCCM = 3110010
030700100712     C                   EVAL      VABFGS = 311
030800100712     C                   EVAL      VATFGS = 311
030900100712     C                   ENDIF
031000110621     C                   IF        %subst(vindta:3:9) = '935193150'             * ANITA
031100110621     C                   EVAL      VABCCM = 3110011
031200110621     C                   EVAL      VATCCM = 3110011
031300110621     C                   EVAL      VABFGS = 311
031400110621     C                   EVAL      VATFGS = 311
031500110621     C                   EVAL      VABCTR = 000
031600110621     C                   ENDIF
031700110621     C                   ENDIF
031800040910     C* ......VABNSP/VATNSP
031900041025     C*                  EVAL      PiStr=%trim(%subst(vindta:21:6))
032000041025     C*                  EXSR      CHKNUM
032100041025     C*                  IF        PiInt=*on
032200041025     C*                  Z-ADD     PiVal         VABNSP
032300041025     C*                  Z-ADD     PiVal         VATNSP
032400041025     C*                  ELSE
032500041025     C*                  ADD       1             errore
032600041025     C*                  EVAL      vinmsg = %trimr(vinmsg)
032700041025     C*                            + ' ' + 'VABNSP/VATNSP'
032800041025     C*                  ENDIF
032900041025     C* NSP => Stacco un numeratore da AZNUM
033000041025     C                   clear                   TRUL33DS
033100041025     C                   eval      I33OPE = *zeros
033200041025     C                   eval      I33CNU = 302
033300041025     C                   eval      I33NUM = 1
033400041025     C                   movel     TRUL33DS      KPJBU
033500041025     C                   call      'TRUL33R'
033600041025     C                   parm                    KPJBA
033700041025     C                   movel     KPJBU         TRUL33DS
033800041025     C                   if        O33ERR = *zeros
033900041025     C                   z-add     O33NRF        VABNSP
034000041025     C                   z-add     O33NRF        VATNSP
034100041025     C                   else
034200041025     C                   Z-ADD     1             errore
034300041025     C                   EVAL      vinmsg = %trimr(vinmsg)
034400041025     C                             + ' ' + 'VABNSP VATNSP'
034500041025     C                   endif
034600040910     C* ......VABRSD
034700040910     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
034800060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
034900060928     C     '@':'A'       XLATE     VABRSD        VABRSD
035000060928     C     '§':'S'       XLATE     VABRSD        VABRSD
035100060928     C     '!':' '       XLATE     VABRSD        VABRSD
035200060928     C     ':':' '       XLATE     VABRSD        VABRSD
035300060928     C*
035400040910     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
035500060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
035600060928     C     '@':'A'       XLATE     VABRD2        VABRD2
035700060928     C     '§':'S'       XLATE     VABRD2        VABRD2
035800060928     C     '!':' '       XLATE     VABRD2        VABRD2
035900060928     C     ':':' '       XLATE     VABRD2        VABRD2
036000040910     C*
036100040910     C                   ENDIF
036200040910     C***
036300040910     C* ...tipo record 12
036400040910     C                   IF        %subst(vindta:1:2) = '12'
036500040910     C* ......VABIND
036600040910     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
036700040910     C* ......VABCAD
036800040910     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
036900040910     C                   EXSR      CHKNUM
037000040910     C                   IF        PiInt=*on
037100040910     C                   Z-ADD     PiVal         Num5_0
037200040910     C                   MOVEL(P)  Num5_0        VABCAD
037300040910     C                   ELSE
037400040910     C                   ADD       1             errore
037500040910     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040910     C                             + ' ' + 'VABCAD'
037700040910     C                   ENDIF
037800040910     C* ......VABLOD
037900040910     C                   EVAL      VABLOD=%trim(%subst(vindta:100:25))
038000040910     C* Reperisco la provincia dal CAP e dalla località
038100040910     C                   IF        VABCAD <> *blanks AND
038200040910     C                             VABPRD  = *blanks
038300040910     C                   CLEAR                   TISI95DS
038400040910     C                   EVAL      I95TCN = '3'
038500040910     C                   Z-ADD     datcor        I95DAT
038600040910     C                   EVAL      I95CAP = VABCAD
038700040910     C                   EVAL      I95LOC = VABLOD
038800040910     C                   CALL      'TISI95R'
038900040910     C                   PARM                    TISI95DS
039000040910     C                   EVAL      VABPRD = O95PRV
039100040910     C                   ENDIF
039200040910     C*
039300040910     C                   ENDIF
039400040910     C***
039500040910     C* ...tipo record 13
039600040910     C                   IF        %subst(vindta:1:2) = '13'
039700050719     C                   Z-ADD     *zeros        VABNCL
039800081222     C* ......VABRMA
039900081222     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
040000040910     C* ......VABNCL
040100050719     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))             * colli
040200040910     C                   EXSR      CHKNUM
040300040910     C                   IF        PiInt=*on
040400050719     C                   ADD       PiVal         VABNCL
040500040910     C                   ELSE
040600040910     C                   ADD       1             errore
040700040910     C                   EVAL      vinmsg = %trimr(vinmsg)
040800040910     C                             + ' ' + 'VABNCL'
040900040910     C                   ENDIF
041000050719     C*
041100050719     C                   EVAL      PiStr=%trim(%subst(vindta:27:2))             * bancali
041200040910     C                   EXSR      CHKNUM
041300040910     C                   IF        PiInt=*on
041400050719     C                   ADD       PiVal         VABNCL
041500040910     C                   ELSE
041600040910     C                   ADD       1             errore
041700040910     C                   EVAL      vinmsg = %trimr(vinmsg)
041800040910     C                             + ' ' + 'VABNCL'
041900040910     C                   ENDIF
042000040910     C* ......VABPKB
042100040910     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
042200040910     C                   EXSR      CHKNUM
042300040910     C                   IF        PiNum=*on
042400040910     C                   EVAL(H)   VABPKB=PiVal/100                             * gestisco 2 dec.
042500040910     C                   ELSE
042600040910     C                   ADD       1             errore
042700040910     C                   EVAL      vinmsg = %trimr(vinmsg)
042800040910     C                             + ' ' + 'VABPKB'
042900040910     C                   ENDIF
043000040929     C* ......VABCBO
043100090701     C                   IF        %subst(vindta:46:1) = '4'
043200040929     C                   EVAL      VABCBO='2'
043300040929     C                   ELSE
043400040929     C                   EVAL      VABCBO='1'
043500040929     C                   ENDIF
043600040910     C* ......VABDCR
043700040910     C                   IF        %subst(vindta:47:6) <> *zeros
043800040910     C                   EVAL      PiStr='20'+%trim(%subst(vindta:47:6))
043900040910     C                   EXSR      CHKNUM
044000040910     C                   IF        PiInt=*on
044100040910     C                   Z-ADD     PiVal         VABDCR
044200040910     C                   ELSE
044300040910     C                   ADD       1             errore
044400040910     C                   EVAL      vinmsg = %trimr(vinmsg)
044500040910     C                             + ' ' + 'VABDCR'
044600040910     C                   ENDIF
044700040910     C                   ENDIF
044800040910     C* ......VABHCR
044900040910     C                   EVAL      PiStr=%trim(%subst(vindta:53:4))
045000040910     C                   EXSR      CHKNUM
045100040910     C                   IF        PiInt=*on
045200040910     C                   Z-ADD     PiVal         VABHCR
045300040910     C                   ELSE
045400040910     C                   ADD       1             errore
045500040910     C                   EVAL      vinmsg = %trimr(vinmsg)
045600040910     C                             + ' ' + 'VABHCR'
045700040910     C                   ENDIF
045800040910     C* ......VABCAS
045900040910     C                   IF        %subst(vindta:57:9) <> *zeros
046000040910     C                   EVAL      FlgCAS = '1'
046100040910     C                   ENDIF
046200040910     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
046300040910     C                   EXSR      CHKNUM
046400040910     C                   IF        PiNum=*on
046500040910     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
046600040910     C                   ELSE
046700040910     C                   ADD       1             errore
046800040910     C                   EVAL      vinmsg = %trimr(vinmsg)
046900040910     C                             + ' ' + 'VABCAS'
047000040910     C                   ENDIF
047100110621     C* ......VABTIC
047200110621     C                   SELECT
047300110621     C                   WHEN      %subst(vindta:66:1) = '3'
047400110621     C                   EVAL      VABTIC=*blanks
047500110621     C                   WHEN      %subst(vindta:66:1) = '5'
047600110621     C                   EVAL      VABTIC='OM'
047700110621     C                   ENDSL
047800040910     C* ......VABVCA
047900040910     C                   EVAL      VABVCA=%trim(%subst(vindta:67:3))
048000040910     C* ......VABIAS
048100060126     C*                  EVAL      PiStr=%trim(%subst(vindta:106:9))
048200060126     C*                  EXSR      CHKNUM
048300060126     C*                  IF        PiNum=*on
048400060126     C*                  EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
048500060126     C*                  ELSE
048600060126     C*                  ADD       1             errore
048700060126     C*                  EVAL      vinmsg = %trimr(vinmsg)
048800060126     C*                            + ' ' + 'VABIAS'
048900060126     C*                  ENDIF
049000040910     C* ......VABVAS
049100040910     C                   EVAL      VABVAS=%trim(%subst(vindta:115:3))
049200040910     C*
049300040910     C                   ENDIF
049400040910     C***
049500040910     C* ...tipo record 14
049600040910     C                   IF        %subst(vindta:1:2) = '14'
049700081222     C* ......VATNOT - "E"
049800040910     C                   EVAL      VATNOT=%trim(%subst(vindta:29:24))
049900040910     C                   EVAL      VATTRC = 'E'
050000040910     C* x ogni tipo record 14 devo scaricare il buffer del file FIVAT
050100040910     C                   WRITE     FIVAT000
050200040910     C*
050300040910     C                   ENDIF
050400041111     C***
050500041111     C* ...tipo record 22
050600041111     C                   IF        %subst(vindta:1:2) = '22'
050700041111     C* ......VABRMO
050800041111     C                   EVAL      VABRMO=%trim(%subst(vindta:3:30))
050900041111     C                   ENDIF
051000060913     C***
051100060914     C* ...tipo record 25
051200071031     C                   IF        %subst(vindta:1:2) = '25'
051300071031     C*
051400071031     C* Solo x traffico MILLIPORE
051500100712     C* => richiesta esplicita Villa del 12/07/2010 => valido x tutti i traffici su LPN 311
051600100712  => C***                IF        VABCCM = 1161200 OR
051700100712     C***                          VABCCM = 3110003
051800100712     C                   IF        VABLNP = 311
051900071031     C*
052000071031     C                   EVAL      dsRT25 = vindta
052100071119     C* Consegna particolare (solo valori ben determinati)
052200071119     C                   IF        dRT25_TC1 = 'A' or
052300071119     C                             dRT25_TC1 = 'P' or
052400071119     C                             dRT25_TC1 = 'S'
052500071031     C                   EVAL      VABTC1 = dRT25_TC1
052600071119     C                   ENDIF
052700060913     C* Telefono destinatario
052800071031     C                   EVAL      VATNOT = dRT25_VATB
052900071031     C                   EVAL      VATTRC = 'B'
053000071031     C                   WRITE     FIVAT000
053100061013     C* Referente consegna
053200071031     C                   EVAL      VATNOT = dRT25_VATA
053300071031     C                   EVAL      VATTRC = 'A'
053400071031     C                   WRITE     FIVAT000
053500060913     C* Data consegna richiesta
053600071219     C                   EVAL      PiStr=dRT25_DCRYYYY+dRT25_DCRMM+dRT25_DCRDD
053700071219     C                   EXSR      CHKNUM
053800071219     C                   IF        PiInt=*on
053900071219     C                   Z-ADD     PiVal         VABDCR
054000071219     C                   ELSE
054100071219     C                   Z-ADD     *zeros        VABDCR
054200071219     C                   ADD       1             errore
054300071219     C                   EVAL      vinmsg = %trimr(vinmsg)
054400071219     C                             + ' ' + 'VABDCR'
054500071219     C                   ENDIF
054600071031     C*
054700100712     C                   ENDIF
054800071031     C*
054900071031     C                   ENDIF
055000040910     C***
055100040910     C* ...tipo record 41
055200040910     C                   IF        %subst(vindta:1:2) = '41'
055300040910     C* ......VABNAS
055400040910     C                   EVAL      VABNAS=%trim(%subst(vindta:3:126))
055500040910     C*
055600040910     C                   ENDIF
055700040910     C***
055800040910     C* ...tipo record 42
055900040910     C                   IF        %subst(vindta:1:2) = '42'
056000040910     C* ......VABNOT/VABNT2
056100040910     C                   EVAL      VABNOT=%trim(%subst(vindta:3:35))
056200040910     C                   EVAL      VABNT2=%trim(%subst(vindta:3+35:35))
056300040910     C*
056400040910     C                   ENDIF
056500040910     C***
056600040910     C* ...tipo record 87
056700040910     C                   IF        %subst(vindta:1:2) = '87'
056800040910     C* ......VABRMA
056900081222     C***                EVAL      VABRMA=%trim(%subst(vindta:21:6))
057000040910     C* ......VABRMN
057100040910     C                   EVAL      PiStr=%trim(%subst(vindta:27:15))
057200040910     C                   EXSR      CHKNUM
057300040910     C                   IF        PiInt=*on
057400040910     C                   Z-ADD     PiVal         VABRMN
057500040910     C                   ELSE
057600040910     C                   Z-ADD     1             VABRMN
057700040910     C                   ADD       1             errore
057800040910     C                   EVAL      vinmsg = %trimr(vinmsg)
057900040910     C                             + ' ' + 'VABRMN'
058000081222     C                   ENDIF
058100081222     C* ......VATNOT - "C"
058200081222     C                   IF        datcor >= 20090101
058300081222     C                   EVAL      VATNOT=%trim(%subst(vindta:27:15))
058400081222     C                   EVAL      VATTRC = 'C'
058500081222     C* x ogni tipo record 87 devo scaricare il buffer del file FIVAT (riferimento Partner)
058600081222     C                   WRITE     FIVAT000
058700081222     C                   ENDIF
058800040910     C*
058900040910     C* Il tipo record 87 "chiude" la sequenza tipi record necessari a comporre 1 ns. bolla
059000040910     C* x cui a questo punto devo scaricare il buffer del file FIVAB quindi...
059100040910     C*
059200040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
059300040910     C                   IF        FlgCAS <> '0'
059400040929     C                   IF        VABCBO = '1'
059500040910     C                   EVAL      VABCBO = '4'
059600040910     C                   ELSE
059700040929     C                   EVAL      VABCBO = '6'
059800040910     C                   ENDIF
059900040929     C                   ENDIF
060000040910     C*
060100040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
060200040910     C                   EXSR      CHKIMPDIV
060300040910     C* ...e in fine...
060400040910     C                   WRITE     FIVAB000
060500040910     C*
060600040910     C                   ENDIF
060700040910     C***
060800010202     C*
060900000801     C* Ebbene...
061000000801     C                   ADD       1             §CTRMO
061100010201     C                   IF        errore <> *zeros
061200000801     C                   ADD       1             §CTRNO
061300000801     C                   EVAL      vinflg = '2'
061400000801     C                   ELSE
061500010201     C                   ADD       1             §CTROKVB
061600000801     C                   ENDIF
061700000801     C*
061800000801     C                   ENDSR
061900010202     C*----------------------------------------------------*
062000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
062100010202     C*----------------------------------------------------*
062200020305     C     PREVAT        BEGSR
062300010202     C*
062400021113     C* Compongo il nome del membro da dare al FIVATWWR
062500010202     C                   eval      parmbr = vlrhdl
062600010202     C                   movel     'M'           parmbr
062700060113     C                   eval      parccm = vlrksc
062800010202     C                   eval      paropz = '1'
062900010202     C* Effettuo la chiamata al CLLE preposto
063000040506     C                   call(e)   'TITVVTC'
063100010202     C                   parm                    parccm
063200010202     C                   parm                    parmbr
063300010202     C                   parm                    paropz
063400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063500010202     C                   if        %error
063600010202     C                   movel     '1'           chkcall
063700010202     C                   else
063800010202     C                   movel     '0'           chkcall
063900010202     C                   endif
064000010202     C*
064100010202     C                   ENDSR
064200000801     C*----------------------------------------------------*
064300000801     C*  CONTROLLO NUMERICITA' CAMPI
064400000801     C*----------------------------------------------------*
064500000801     C     CHKNUM        BEGSR
064600081222     C*
064700081222     C                   IF        PiDecChr = *blanks
064800081222     C                   EVAL      PiDecChr = ','
064900081222     C                   ENDIF
065000081222     C*
065100081222     C                   callp(e)  UBISNUM_Check(PiStr
065200081222     C                                          :PiDecChr
065300081222     C                                          :PiVal
065400081222     C                                          :PiNum
065500081222     C                                          :PiInt)
065600081222     C*
065700081222     C                   IF        %error
065800081222     C                   EVAL      PiInt=*off
065900081222     C                   ENDIF
066000000801     C*
066100000801     C                   ENDSR
066200000801     C***
066300000801
066400011113
066500011113     C*----------------------------------------------------*
066600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066700011113     C*----------------------------------------------------*
066800011113     C     CHKIMPDIV     BEGSR
066900011113     C*
067000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067100011113     C                   Z-ADD     *zeros        wrkDec            9 9
067200011113     C*
067300011113     C* Come prima cosa effettuo considerazioni sulla divisa
067400011113     C                   IF        vabIAS > *zeros
067500011113     C                   IF        vabVAS <> 'EUR'
067600011113     C                   EVAL      vabVAS =  'ITL'
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C                   IF        vabCAS > *zeros
068100011113     C                   IF        vabVCA <> 'EUR'
068200011113     C                   EVAL      vabVCA =  'ITL'
068300011113     C                   ENDIF
068400011113     C                   ENDIF
068500011113     C*
068600011113     C                   IF        vabVMD > *zeros
068700020305     C                   IF        vabVAD <> 'EUR'
068800011113     C                   EVAL      vabVAD =  'ITL'
068900011113     C                   ENDIF
069000011113     C                   ENDIF
069100011113     C*
069200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069300011113     C                   Z-ADD     vabIAS        wrkDec
069400011113     C                   IF        wrkDec > *zeros
069500011113     C                   IF        vabVAS = 'ITL'
069600011113     C                   EVAL      vabIAS = *zeros
069700011113     C                   ENDIF
069800011113     C                   ENDIF
069900011113     C*
070000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
070100011113     C                   Z-ADD     vabCAS        wrkDec
070200011113     C                   IF        wrkDec > *zeros
070300011113     C                   IF        vabVCA = 'ITL'
070400011113     C                   EVAL      vabCAS = *zeros
070500011113     C                   ENDIF
070600011113     C                   ENDIF
070700011113     C*
070800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070900011113     C                   Z-ADD     vabVMD        wrkDec
071000011113     C                   IF        wrkDec > *zeros
071100011113     C                   IF        vabVAD = 'ITL'
071200011113     C                   EVAL      vabVMD = *zeros
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C                   ENDSR
071700011113     C***
071800011113
071900011113
072000000801
072100000801
072200990920      /TITLE Invio dei dati al punto operativo.
072300010202     C     invio         BEGSR
072400990920     C*
072500021113     C* 1° invio FIVAT
072600010201     C                   reset                   dscmz
072700010201     C                   move      vlrpoi        cmzdst
072800021113     C                   eval      cmzfld = 'FIVATWWR'
072900010201     C                   eval      cmzmbd = vlrhdl
073000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073100021009     C***                if        prmfir = *blanks
073200021113     C                   eval      cmzfla = 'FIVAT00F'
073300021113     C                   eval      cmzmba = 'FIVAT00F'
073400021009     C***                else
073500021009     C***                eval      cmzfla = prmfir
073600021009     C***                eval      cmzmba = prmfir
073700021009     C***                endif
073800010201     C                   eval      cmznrr = *zeros
073900020305     C                   move      §ctrokvt      cmznrr
074000021018     C                   eval      cmzlba = vlrfl1
074100010201     C                   call(e)   'TIS711C'
074200010201     C                   parm                    dscmz
074300010201     C                   parm      *blanks       esito
074400010205     C                   if        %error
074500010205     C                             or cmzerr = '1'
074600010205     C                             or esito  = '1'
074700010205     C                   eval      wrkesito = '3'
074800010205     C                   else
074900010201     C*
075000021113     C* 2° invio FIVAB
075100010201     C                   reset                   dscmz
075200010201     C                   move      vlrpoi        cmzdst
075300010201     C                   eval      cmzfld = vlrfou
075400010201     C                   eval      cmzmbd = vlrhdl
075500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075600021009     C***                if        prmfir = *blanks
075700021113     C                   eval      cmzfla = 'FIVAB00F'
075800021113     C                   eval      cmzmba = 'FIVAB00F'
075900021009     C***                else
076000021009     C***                eval      cmzfla = prmfir
076100021009     C***                eval      cmzmba = prmfir
076200021009     C***                endif
076300010201     C                   eval      cmznrr = *zeros
076400010201     C                   move      §ctrokvb      cmznrr
076500021018     C                   eval      cmzlba = vlrfl1
076600010201     C                   call(e)   'TIS711C'
076700010201     C                   parm                    dscmz
076800010201     C                   parm      *blanks       esito
076900010201     C                   if        %error
077000010201     C                             or cmzerr = '1'
077100010201     C                             or esito  = '1'
077200010201     C                   eval      wrkesito = '3'
077300010201     C                   endif
077400010205     C                   endif
077500990920     C*
077600000613     C                   ENDSR
077700000613     C***
077800070411
077900071219     C     *pssr         BEGSR
078000070411     C*
078100071219     C                   if        %open(tivin00r)
078200071219     C                   close     tivin00r
078300071219     C                   endif
078400071219     C                   if        %open(fivabwwr)
078500071219     C                   close     fivabwwr
078600071219     C                   endif
078700071219     C                   if        %open(fivatwwr)
078800071219     C                   close     fivatwwr
078900071219     C                   endif
079000070411     C*
079100070411     C* Effettuo la chiamata al CLLE preposto
079200071219     C                   call(e)   'TITVVTC'
079300071219     C                   parm                    parccm
079400071219     C                   parm                    parmbr
079500071219     C                   parm      '2'           paropz
079600071219     C*
079700071219     C                   eval      wrkesito = '2'
079800071219     C*
079900071219     C                   seton                                        LR
080000070411     C*
080100071219     C                   ENDSR     '*CANCL'
080200070411     C***
080300070411
080400990910
080500000613     C     *inzsr        BEGSR
080600990910     C*
080700990910     C     *entry        plist
080800990920     C                   parm                    tivlrds
080900990921     C                   parm      wrkesito      esito
081000000724     C                   parm                    prmlit
081100000710     C                   parm                    prmfir
081200000613     C*
081300000830     C* CALCOLA LA DATA CORRENTE
081400000830     C                   time                    wn14             14 0
081500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
081600000830     C                   z-add     wn8           g08dat
081700000830     C                   z-add     *zeros        g08inv
081800000830     C                   movel     '0'           g08err
081900000830     C                   call      'XSRDA8'
082000000830     C                   parm                    wlbda8
082100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
082200000830     C*
082300000613     C                   ENDSR
082400000613     C***
