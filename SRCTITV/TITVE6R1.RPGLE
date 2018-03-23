000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300120207     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800120207     FTIVGD00F  O    E             DISK
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990920     D dscmz         e ds                  inz
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600990920     D tivlrds       e ds                  extname(tivlr00f)
001700040506     D tisi95ds      e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700120130     D wOut            s              1    INZ('0')
002800120207     D wNomeFile       s             30    INZ(*blanks)
002900000830
003000041025     D*------------------
003100041025     D* DS REPERIMENTO NUMERATORE
003200041025     D*------------------
003300041025     D trul33ds      e ds                  inz
003400041025     D*------------------
003500041025     D* DS ARCHITETTURA
003600041025     D*------------------
003700041025     D kpjba         e ds                  inz
003800060913     D*------------------
003900060913     D* DS RIDEFINIZIONE TIPO RECORD CLIENTE '25' - Informazioni aggiuntive
004000060913     D*------------------
004100060913     D dsRT25          ds                  inz
004200060928     D  dRT25_RT25                    2
004300060913     D  dRT25_TC1                     1
004400060913     D  dRT25_VATB                   25
004500071031     D  dRT25_VATA                   26
004600060913     D  dRT25_DCRDD                   2
004700060913     D  dRT25_DCRMM                   2
004800060913     D  dRT25_DCRYYYY                 4
004900060913     D*------------------
005000990908
005100081222
005200081222     D*------------------
005300081222     D* LINKING A DEFINIZIONI ESTERNE
005400081222     D*------------------
005500081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005600081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700010201
005800010201
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000613     C*
006300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006400000613     C*
006500010202     C* Effettuo la chiamata al CLLE preposto
006600040506     C                   call(e)   'TITVVTC'
006700010202     C                   parm                    parccm
006800010202     C                   parm                    parmbr
006900010202     C                   parm      '2'           paropz
007000040929     C*
007100040929     C* Esegue lancio TISI95R solo x chiusura
007200040929     C                   CLEAR                   TISI95DS
007300040929     C                   EVAL      I95TLA = 'C'
007400040929     C                   CALL      'TISI95R'
007500040929     C                   PARM                    TISI95DS
007600000616     C*
007700010201     C                   seton                                        LR
007800990908
007900000801
008000910830     C*--------------------------------------------------------
008100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008200910830     C*--------------------------------------------------------
008300040526     C     RWFILE        BEGSR
008400990910     C*
008500990914     C                   if        not %open(tivin00r)
008600990908     C                   open      tivin00r
008700990914     C                   endif
008800021113     C                   if        not %open(fivabwwr)
008900021113     C                   open      fivabwwr
009000990914     C                   endif
009100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009200020305     C                   exsr      prevat
009300010201     C*
009400010202     C                   if        chkcall = '0'
009500010202     C*
009600021113     C                   if        not %open(fivatwwr)
009700021113     C                   open      fivatwwr
009800010201     C                   endif
009900990910     C*
010000010201     C                   clear                   §CTROKVB          5 0
010100020305     C                   clear                   §CTROKVT          5 0
010200000801     C                   clear                   §CTRMO            5 0
010300000801     C                   clear                   §CTRNO            5 0
010400040910     C*
010500921023     C                   DO        *HIVAL
010600990913     C*
010700990915     C                   READ      tivin00r                               70
010800040910     C                   if        vindta > *blanks
010900000613     C                   add       1             rrnum
011000000801     C*
011100000801     C                   if        *in70 = *off
011200000801     C                             and
011300000801     C                             (vinflg = *blanks
011400000801     C                              or vinflg = '0'
011500000801     C                              or vinflg = '2')
011600000801     C*
011700000801     C                   clear                   vinmsg
011800000801     C                   eval      vinflg = '1'
011900040910     C*
012000040910     C* Eseguo routine d traduzione
012100040910     C                   exsr      impvabvat
012200040802     C*
012300010305     C                   endif
012400000905     C*
012500000905     C                   else
012600000905     C                   eval      vinflg = '1'
012700000905     C                   endif
012800000905     C*
012900000905     C  N70              update    tivin000
013000000905     C*
013100991022     C  N70              ENDdo
013200010202     C*
013300120130     C* Scarico il buffer d output in sospeso ...
013400120130     C                   WRITE     FIVAB000
013500120207     C                   exsr      wrivgd
013600120130     C*
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
018600120130     C                   Z-ADD     2221280       VABCCM
018700120130     C                   Z-ADD     2221280       VATCCM
018800120130     C                   Z-ADD     222           VABLNP
018900120130     C                   Z-ADD     222           VATLNP
019000120130     C                   Z-ADD     003           VABCTR
019100040714     C                   MOVEL     '7Q'          VABCTM
019200040823     C                   MOVEL     '1'           VABCBO
019300060206     C                   MOVEL     'D'           VABTSP
019400020619     C* ... e poi verifico se sono stati passati come parametri
019500020619     C                   IF        vlrppt > *blanks
019600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019800020619     C                   EXSR      CHKNUM
019900020619     C                   IF        PiInt=*on
020000020619     C                   Z-ADD     PiVal         VABCCM
020100020619     C                   Z-ADD     PiVal         VATCCM
020200020619     C                   ENDIF
020300040506     C                   ENDIF
020400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020600020619     C                   EXSR      CHKNUM
020700020619     C                   IF        PiInt=*on
020800020619     C                   Z-ADD     PiVal         VABLNP
020900020619     C                   Z-ADD     PiVal         VATLNP
021000040506     C                   ENDIF
021100020619     C                   ENDIF
021200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021400020619     C                   EXSR      CHKNUM
021500020619     C                   IF        PiInt=*on
021600020619     C                   Z-ADD     PiVal         VABCTR
021700040506     C                   ENDIF
021800020619     C                   ENDIF
021900070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
022000070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
022100070427     C                   ENDIF
022200020619     C                   ENDIF
022300000801     C*
022400000801     C                   ENDSR
022500000801     C*----------------------------------------------------*
022600040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
022700000801     C*----------------------------------------------------*
022800040910     C     IMPVABVAT     BEGSR
022900040910     C*
023000040910     C* Traduzione relativa ai tipi record del file del cliente
023100040910     C*
023200040910     C***
023300040910     C* ...tipo record 11
023400040910     C                   IF        %subst(vindta:1:2) = '11'
023500120130     C*
023600120130     C* Se buffer da scaricare => scarico ...
023700120130     C                   IF        wOut = '1'
023800120130     C                   WRITE     FIVAB000
023900120207     C                   exsr      wrivgd
024000120130     C                   ENDIF
024100120130     C*
024200120130     C                   EVAL      wOut = '1'
024300120130     C*
024400040910     C* ......inizializzazioni iniziali e formati record file Bartolini
024500040910     C                   EXSR      INZVAR
024600040910     C                   EXSR      DEFCAM
024700081222     C*
024800081222     C                   Z-ADD     *zeros        errore            1 0
024900081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
025000081222     C                   MOVEL     datcor        VABAAS
025100081222     C                   MOVEL     datcor        VATAAS
025200081222     C                   MOVE      datcor        VABMGS
025300081222     C                   MOVE(P)   vlrpoi        VABFGS
025400081222     C                   MOVE(P)   vlrpoi        VATFGS
025500071011     C*
025600120130     C****               IF        %subst(vindta:3:9) = '934397260'             * MILLIPORE
025700120130     C****               EVAL      VABCCM = 3110003
025800120130     C****               EVAL      VATCCM = 3110003
025900120130     C****               EVAL      VABFGS = 311
026000120130     C****               EVAL      VATFGS = 311
026100120130     C****               ENDIF
026200120130     C*
026300040910     C* ......VABNSP/VATNSP
026400041025     C*                  EVAL      PiStr=%trim(%subst(vindta:21:6))
026500041025     C*                  EXSR      CHKNUM
026600041025     C*                  IF        PiInt=*on
026700041025     C*                  Z-ADD     PiVal         VABNSP
026800041025     C*                  Z-ADD     PiVal         VATNSP
026900041025     C*                  ELSE
027000041025     C*                  ADD       1             errore
027100041025     C*                  EVAL      vinmsg = %trimr(vinmsg)
027200041025     C*                            + ' ' + 'VABNSP/VATNSP'
027300041025     C*                  ENDIF
027400041025     C* NSP => Stacco un numeratore da AZNUM
027500041025     C                   clear                   TRUL33DS
027600041025     C                   eval      I33OPE = *zeros
027700041025     C                   eval      I33CNU = 302
027800041025     C                   eval      I33NUM = 1
027900041025     C                   movel     TRUL33DS      KPJBU
028000041025     C                   call      'TRUL33R'
028100041025     C                   parm                    KPJBA
028200041025     C                   movel     KPJBU         TRUL33DS
028300041025     C                   if        O33ERR = *zeros
028400041025     C                   z-add     O33NRF        VABNSP
028500041025     C                   z-add     O33NRF        VATNSP
028600041025     C                   else
028700041025     C                   Z-ADD     1             errore
028800041025     C                   EVAL      vinmsg = %trimr(vinmsg)
028900041025     C                             + ' ' + 'VABNSP VATNSP'
029000041025     C                   endif
029100040910     C* ......VABRSD
029200040910     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
029300060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
029400060928     C     '@':'A'       XLATE     VABRSD        VABRSD
029500060928     C     '§':'S'       XLATE     VABRSD        VABRSD
029600060928     C     '!':' '       XLATE     VABRSD        VABRSD
029700060928     C     ':':' '       XLATE     VABRSD        VABRSD
029800060928     C*
029900040910     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
030000060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
030100060928     C     '@':'A'       XLATE     VABRD2        VABRD2
030200060928     C     '§':'S'       XLATE     VABRD2        VABRD2
030300060928     C     '!':' '       XLATE     VABRD2        VABRD2
030400060928     C     ':':' '       XLATE     VABRD2        VABRD2
030500040910     C*
030600040910     C                   ENDIF
030700040910     C***
030800040910     C* ...tipo record 12
030900040910     C                   IF        %subst(vindta:1:2) = '12'
031000040910     C* ......VABIND
031100040910     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
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
033700040910     C*
033800040910     C                   ENDIF
033900040910     C***
034000040910     C* ...tipo record 13
034100040910     C                   IF        %subst(vindta:1:2) = '13'
034200050719     C                   Z-ADD     *zeros        VABNCL
034300081222     C* ......VABRMA
034400081222     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
034500120130     C* ......VABRMN
034600120130     C                   EVAL      PiStr=%trim(%subst(vindta:80+4:15-4))
034700120130     C                   EXSR      CHKNUM
034800120130     C                   IF        PiInt=*on
034900120130     C                   ADD       PiVal         VABRMN
035000120130     C                   ELSE
035100120130     C                   ADD       1             errore
035200120130     C                   EVAL      vinmsg = %trimr(vinmsg)
035300120130     C                             + ' ' + 'VABRMN'
035400120130     C                   ENDIF
035500040910     C* ......VABNCL
035600050719     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))             * colli
035700040910     C                   EXSR      CHKNUM
035800040910     C                   IF        PiInt=*on
035900050719     C                   ADD       PiVal         VABNCL
036000040910     C                   ELSE
036100040910     C                   ADD       1             errore
036200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
036300040910     C                             + ' ' + 'VABNCL'
036400040910     C                   ENDIF
036500050719     C*
036600050719     C                   EVAL      PiStr=%trim(%subst(vindta:27:2))             * bancali
036700040910     C                   EXSR      CHKNUM
036800040910     C                   IF        PiInt=*on
036900050719     C                   ADD       PiVal         VABNCL
037000040910     C                   ELSE
037100040910     C                   ADD       1             errore
037200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
037300040910     C                             + ' ' + 'VABNCL'
037400040910     C                   ENDIF
037500040910     C* ......VABPKB
037600040910     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
037700040910     C                   EXSR      CHKNUM
037800040910     C                   IF        PiNum=*on
037900040910     C                   EVAL(H)   VABPKB=PiVal/100                             * gestisco 2 dec.
038000040910     C                   ELSE
038100040910     C                   ADD       1             errore
038200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
038300040910     C                             + ' ' + 'VABPKB'
038400040910     C                   ENDIF
038500040929     C* ......VABCBO
038600090701     C                   IF        %subst(vindta:46:1) = '4'
038700040929     C                   EVAL      VABCBO='2'
038800040929     C                   ELSE
038900040929     C                   EVAL      VABCBO='1'
039000040929     C                   ENDIF
039100040910     C* ......VABDCR
039200040910     C                   IF        %subst(vindta:47:6) <> *zeros
039300040910     C                   EVAL      PiStr='20'+%trim(%subst(vindta:47:6))
039400040910     C                   EXSR      CHKNUM
039500040910     C                   IF        PiInt=*on
039600040910     C                   Z-ADD     PiVal         VABDCR
039700040910     C                   ELSE
039800040910     C                   ADD       1             errore
039900040910     C                   EVAL      vinmsg = %trimr(vinmsg)
040000040910     C                             + ' ' + 'VABDCR'
040100040910     C                   ENDIF
040200040910     C                   ENDIF
040300040910     C* ......VABHCR
040400040910     C                   EVAL      PiStr=%trim(%subst(vindta:53:4))
040500040910     C                   EXSR      CHKNUM
040600040910     C                   IF        PiInt=*on
040700040910     C                   Z-ADD     PiVal         VABHCR
040800040910     C                   ELSE
040900040910     C                   ADD       1             errore
041000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
041100040910     C                             + ' ' + 'VABHCR'
041200040910     C                   ENDIF
041300040910     C* ......VABCAS
041400040910     C                   IF        %subst(vindta:57:9) <> *zeros
041500040910     C                   EVAL      FlgCAS = '1'
041600040910     C                   ENDIF
041700040910     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
041800040910     C                   EXSR      CHKNUM
041900040910     C                   IF        PiNum=*on
042000040910     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
042100040910     C                   ELSE
042200040910     C                   ADD       1             errore
042300040910     C                   EVAL      vinmsg = %trimr(vinmsg)
042400040910     C                             + ' ' + 'VABCAS'
042500040910     C                   ENDIF
042600110621     C* ......VABTIC
042700110621     C                   SELECT
042800110621     C                   WHEN      %subst(vindta:66:1) = '3'
042900110621     C                   EVAL      VABTIC=*blanks
043000110621     C                   WHEN      %subst(vindta:66:1) = '5'
043100110621     C                   EVAL      VABTIC='OM'
043200110621     C                   ENDSL
043300040910     C* ......VABVCA
043400040910     C                   EVAL      VABVCA=%trim(%subst(vindta:67:3))
043500040910     C* ......VABIAS
043600060126     C*                  EVAL      PiStr=%trim(%subst(vindta:106:9))
043700060126     C*                  EXSR      CHKNUM
043800060126     C*                  IF        PiNum=*on
043900060126     C*                  EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
044000060126     C*                  ELSE
044100060126     C*                  ADD       1             errore
044200060126     C*                  EVAL      vinmsg = %trimr(vinmsg)
044300060126     C*                            + ' ' + 'VABIAS'
044400060126     C*                  ENDIF
044500040910     C* ......VABVAS
044600040910     C                   EVAL      VABVAS=%trim(%subst(vindta:115:3))
044700040910     C*
044800040910     C                   ENDIF
044900040910     C***
045000040910     C* ...tipo record 14
045100040910     C                   IF        %subst(vindta:1:2) = '14'
045200081222     C* ......VATNOT - "E"
045300040910     C                   EVAL      VATNOT=%trim(%subst(vindta:29:24))
045400040910     C                   EVAL      VATTRC = 'E'
045500040910     C* x ogni tipo record 14 devo scaricare il buffer del file FIVAT
045600040910     C                   WRITE     FIVAT000
045700040910     C*
045800040910     C                   ENDIF
045900041111     C***
046000041111     C* ...tipo record 22
046100041111     C                   IF        %subst(vindta:1:2) = '22'
046200041111     C* ......VABRMO
046300041111     C                   EVAL      VABRMO=%trim(%subst(vindta:3:30))
046400041111     C                   ENDIF
046500060913     C***
046600060914     C* ...tipo record 25
046700071031     C                   IF        %subst(vindta:1:2) = '25'
046800071031     C*
046900071031     C* Solo x traffico MILLIPORE
047000100712     C* => richiesta esplicita Villa del 12/07/2010 => valido x tutti i traffici su LPN 311
047100100712  => C***                IF        VABCCM = 1161200 OR
047200100712     C***                          VABCCM = 3110003
047300100712     C                   IF        VABLNP = 311
047400071031     C*
047500071031     C                   EVAL      dsRT25 = vindta
047600071119     C* Consegna particolare (solo valori ben determinati)
047700071119     C                   IF        dRT25_TC1 = 'A' or
047800071119     C                             dRT25_TC1 = 'P' or
047900071119     C                             dRT25_TC1 = 'S'
048000071031     C                   EVAL      VABTC1 = dRT25_TC1
048100071119     C                   ENDIF
048200060913     C* Telefono destinatario
048300071031     C                   EVAL      VATNOT = dRT25_VATB
048400071031     C                   EVAL      VATTRC = 'B'
048500071031     C                   WRITE     FIVAT000
048600061013     C* Referente consegna
048700071031     C                   EVAL      VATNOT = dRT25_VATA
048800071031     C                   EVAL      VATTRC = 'A'
048900071031     C                   WRITE     FIVAT000
049000060913     C* Data consegna richiesta
049100071219     C                   EVAL      PiStr=dRT25_DCRYYYY+dRT25_DCRMM+dRT25_DCRDD
049200071219     C                   EXSR      CHKNUM
049300071219     C                   IF        PiInt=*on
049400071219     C                   Z-ADD     PiVal         VABDCR
049500071219     C                   ELSE
049600071219     C                   Z-ADD     *zeros        VABDCR
049700071219     C                   ADD       1             errore
049800071219     C                   EVAL      vinmsg = %trimr(vinmsg)
049900071219     C                             + ' ' + 'VABDCR'
050000071219     C                   ENDIF
050100071031     C*
050200100712     C                   ENDIF
050300071031     C*
050400071031     C                   ENDIF
050500040910     C***
050600040910     C* ...tipo record 41
050700040910     C                   IF        %subst(vindta:1:2) = '41'
050800040910     C* ......VABNAS
050900040910     C                   EVAL      VABNAS=%trim(%subst(vindta:3:126))
051000040910     C*
051100040910     C                   ENDIF
051200040910     C***
051300040910     C* ...tipo record 42
051400040910     C                   IF        %subst(vindta:1:2) = '42'
051500040910     C* ......VABNOT/VABNT2
051600040910     C                   EVAL      VABNOT=%trim(%subst(vindta:3:35))
051700040910     C                   EVAL      VABNT2=%trim(%subst(vindta:3+35:35))
051800040910     C*
051900040910     C                   ENDIF
052000040910     C***
052100010202     C*
052200000801     C* Ebbene...
052300000801     C                   ADD       1             §CTRMO
052400010201     C                   IF        errore <> *zeros
052500000801     C                   ADD       1             §CTRNO
052600000801     C                   EVAL      vinflg = '2'
052700000801     C                   ELSE
052800010201     C                   ADD       1             §CTROKVB
052900000801     C                   ENDIF
053000000801     C*
053100000801     C                   ENDSR
053200010202     C*----------------------------------------------------*
053300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053400010202     C*----------------------------------------------------*
053500020305     C     PREVAT        BEGSR
053600010202     C*
053700021113     C* Compongo il nome del membro da dare al FIVATWWR
053800010202     C                   eval      parmbr = vlrhdl
053900010202     C                   movel     'M'           parmbr
054000060113     C                   eval      parccm = vlrksc
054100010202     C                   eval      paropz = '1'
054200010202     C* Effettuo la chiamata al CLLE preposto
054300040506     C                   call(e)   'TITVVTC'
054400010202     C                   parm                    parccm
054500010202     C                   parm                    parmbr
054600010202     C                   parm                    paropz
054700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054800010202     C                   if        %error
054900010202     C                   movel     '1'           chkcall
055000010202     C                   else
055100010202     C                   movel     '0'           chkcall
055200010202     C                   endif
055300010202     C*
055400010202     C                   ENDSR
055500000801     C*----------------------------------------------------*
055600000801     C*  CONTROLLO NUMERICITA' CAMPI
055700000801     C*----------------------------------------------------*
055800000801     C     CHKNUM        BEGSR
055900081222     C*
056000081222     C                   IF        PiDecChr = *blanks
056100081222     C                   EVAL      PiDecChr = ','
056200081222     C                   ENDIF
056300081222     C*
056400081222     C                   callp(e)  UBISNUM_Check(PiStr
056500081222     C                                          :PiDecChr
056600081222     C                                          :PiVal
056700081222     C                                          :PiNum
056800081222     C                                          :PiInt)
056900081222     C*
057000081222     C                   IF        %error
057100081222     C                   EVAL      PiInt=*off
057200081222     C                   ENDIF
057300000801     C*
057400000801     C                   ENDSR
057500000801     C***
057600000801
057700011113
057800011113     C*----------------------------------------------------*
057900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058000011113     C*----------------------------------------------------*
058100011113     C     CHKIMPDIV     BEGSR
058200011113     C*
058300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058400011113     C                   Z-ADD     *zeros        wrkDec            9 9
058500011113     C*
058600011113     C* Come prima cosa effettuo considerazioni sulla divisa
058700011113     C                   IF        vabIAS > *zeros
058800011113     C                   IF        vabVAS <> 'EUR'
058900011113     C                   EVAL      vabVAS =  'ITL'
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   IF        vabCAS > *zeros
059400011113     C                   IF        vabVCA <> 'EUR'
059500011113     C                   EVAL      vabVCA =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C                   IF        vabVMD > *zeros
060000020305     C                   IF        vabVAD <> 'EUR'
060100011113     C                   EVAL      vabVAD =  'ITL'
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060600011113     C                   Z-ADD     vabIAS        wrkDec
060700011113     C                   IF        wrkDec > *zeros
060800011113     C                   IF        vabVAS = 'ITL'
060900011113     C                   EVAL      vabIAS = *zeros
061000011113     C                   ENDIF
061100011113     C                   ENDIF
061200011113     C*
061300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061400011113     C                   Z-ADD     vabCAS        wrkDec
061500011113     C                   IF        wrkDec > *zeros
061600011113     C                   IF        vabVCA = 'ITL'
061700011113     C                   EVAL      vabCAS = *zeros
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062200011113     C                   Z-ADD     vabVMD        wrkDec
062300011113     C                   IF        wrkDec > *zeros
062400011113     C                   IF        vabVAD = 'ITL'
062500011113     C                   EVAL      vabVMD = *zeros
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C                   ENDSR
063000011113     C***
063100011113
063200011113
063300000801
063400000801
063500990920      /TITLE Invio dei dati al punto operativo.
063600010202     C     invio         BEGSR
063700990920     C*
063800021113     C* 1° invio FIVAT
063900010201     C                   reset                   dscmz
064000010201     C                   move      vlrpoi        cmzdst
064100021113     C                   eval      cmzfld = 'FIVATWWR'
064200010201     C                   eval      cmzmbd = vlrhdl
064300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064400021009     C***                if        prmfir = *blanks
064500021113     C                   eval      cmzfla = 'FIVAT00F'
064600021113     C                   eval      cmzmba = 'FIVAT00F'
064700021009     C***                else
064800021009     C***                eval      cmzfla = prmfir
064900021009     C***                eval      cmzmba = prmfir
065000021009     C***                endif
065100010201     C                   eval      cmznrr = *zeros
065200020305     C                   move      §ctrokvt      cmznrr
065300021018     C                   eval      cmzlba = vlrfl1
065400010201     C                   call(e)   'TIS711C'
065500010201     C                   parm                    dscmz
065600010201     C                   parm      *blanks       esito
065700010205     C                   if        %error
065800010205     C                             or cmzerr = '1'
065900010205     C                             or esito  = '1'
066000010205     C                   eval      wrkesito = '3'
066100010205     C                   else
066200010201     C*
066300021113     C* 2° invio FIVAB
066400010201     C                   reset                   dscmz
066500010201     C                   move      vlrpoi        cmzdst
066600010201     C                   eval      cmzfld = vlrfou
066700010201     C                   eval      cmzmbd = vlrhdl
066800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066900021009     C***                if        prmfir = *blanks
067000021113     C                   eval      cmzfla = 'FIVAB00F'
067100021113     C                   eval      cmzmba = 'FIVAB00F'
067200021009     C***                else
067300021009     C***                eval      cmzfla = prmfir
067400021009     C***                eval      cmzmba = prmfir
067500021009     C***                endif
067600010201     C                   eval      cmznrr = *zeros
067700010201     C                   move      §ctrokvb      cmznrr
067800021018     C                   eval      cmzlba = vlrfl1
067900010201     C                   call(e)   'TIS711C'
068000010201     C                   parm                    dscmz
068100010201     C                   parm      *blanks       esito
068200010201     C                   if        %error
068300010201     C                             or cmzerr = '1'
068400010201     C                             or esito  = '1'
068500010201     C                   eval      wrkesito = '3'
068600010201     C                   endif
068700010205     C                   endif
068800990920     C*
068900000613     C                   ENDSR
069000000613     C***
069100070411
069200071219     C     *pssr         BEGSR
069300070411     C*
069400071219     C                   if        %open(tivin00r)
069500071219     C                   close     tivin00r
069600071219     C                   endif
069700071219     C                   if        %open(fivabwwr)
069800071219     C                   close     fivabwwr
069900071219     C                   endif
070000071219     C                   if        %open(fivatwwr)
070100071219     C                   close     fivatwwr
070200071219     C                   endif
070300070411     C*
070400070411     C* Effettuo la chiamata al CLLE preposto
070500071219     C                   call(e)   'TITVVTC'
070600071219     C                   parm                    parccm
070700071219     C                   parm                    parmbr
070800071219     C                   parm      '2'           paropz
070900071219     C*
071000071219     C                   eval      wrkesito = '2'
071100071219     C*
071200071219     C                   seton                                        LR
071300070411     C*
071400071219     C                   ENDSR     '*CANCL'
071500070411     C***
071600120207
071700120207
071800120207
071900120207      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
072000120207     C     wriVGD        BEGSR
072100120207     C*
072200120207     C                   clear                   tivgd000
072300120207     C                   eval      vgdDTA = %trim(wNomeFile)       +';'+
072400120207     C                                      %trim(vabRSD)          +';'+
072500120207     C                                      %trim(vabIND)          +';'+
072600120207     C                                      %trim(vabLOD)          +';'+
072700120207     C                                      %trim(vabCAD)          +';'+
072800120207     C                                      %trim(vabRMA)          +';'+
072900120207     C                                      %trim(%char(vabNCL))   +';'+
073000120207     C                                      %trim(%editw(vabPKB:'    0 , '))
073100120207     C                   eval      vgdTIP = 'EB'
073200120207     C                   eval      vgdKSU = vlrKSC
073300120207     C                   eval      vgdTSC = 'WW'
073400120207     C                   eval      vgdDAT = datcor
073500120207     C                   eval      vgdPGM = 'TITVE6R1'
073600120207     C                   write     tivgd000
073700120207     C*
073800120207     C                   ENDSR
073900120207     C*------------------------------------------------------------------------*
074000120207
074100070411
074200990910
074300000613     C     *inzsr        BEGSR
074400990910     C*
074500990910     C     *entry        plist
074600990920     C                   parm                    tivlrds
074700990921     C                   parm      wrkesito      esito
074800000724     C                   parm                    prmlit
074900000710     C                   parm                    prmfir
075000120207     C*
075100120207     C                   MOVE(P)   vlrMSG        wNomeFile
075200000613     C*
075300000830     C* CALCOLA LA DATA CORRENTE
075400120207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075500120207     C                   eval      datcor = %dec(%date() : *iso)
075600000830     C*
075700000613     C                   ENDSR
075800000613     C***
