000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800040910     Ftitve6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000071219     F                                     infsr(*pssr)
001100040910     Ftitve6ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300071219     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600040506     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900041025     D*------------------
005000041025     D* DS REPERIMENTO NUMERATORE
005100041025     D*------------------
005200041025     D trul33ds      e ds                  inz
005300041025     D*------------------
005400041025     D* DS ARCHITETTURA
005500041025     D*------------------
005600041025     D kpjba         e ds                  inz
005700060913     D*------------------
005800060913     D* DS RIDEFINIZIONE TIPO RECORD CLIENTE '25' - Informazioni aggiuntive
005900060913     D*------------------
006000060913     D dsRT25          ds                  inz
006100060928     D  dRT25_RT25                    2
006200060913     D  dRT25_TC1                     1
006300060913     D  dRT25_VATB                   25
006400071031     D  dRT25_VATA                   26
006500060913     D  dRT25_DCRDD                   2
006600060913     D  dRT25_DCRMM                   2
006700060913     D  dRT25_DCRYYYY                 4
006800060913     D*------------------
006900990908
007000010201
007100010201
007200990915     C                   time                    wrkdata
007300990915     C                   time                    wrkora
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000724     C*
007800000724     C* SE OCCORRE SPEDIRE IN FILIALE
007900000724     C                   if        vlrpoi <> *zeros
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000724     C     vlrpoi        chain     azorg01l
008400000724     C                   if        %found
008500000616     C                   movel(p)  CMD(1)        depcmd
008600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
009500000724     C                   endif
009600000616     C*
009700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009800000616     C                   movel(p)  CMD(2)        depcmd
009900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010000000616     C*
010100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010200011113     C                   Z-ADD     150           LENGH            15 5
010300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010400000616     C                   PARM                    depcmd
010500000616     C                   PARM                    LENGH
010600000616     C*
010700040910     C                   if        not %open(titve6ps)
010800040910     C                   open      titve6ps
010900000616     C                   except    testdett
011000000613     C                   endif
011100000613     C*
011200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011400000613     C*
011500010202     C* Effettuo la chiamata al CLLE preposto
011600040506     C                   call(e)   'TITVVTC'
011700010202     C                   parm                    parccm
011800010202     C                   parm                    parmbr
011900010202     C                   parm      '2'           paropz
012000010202     C*
012100040910     C                   if        %open(titve6ps)
012200000616     C                   except    findett
012300040910     C                   close     titve6ps
012400000613     C                   endif
012500000616     C*
012600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    CMD(3)
013000000616     C                   PARM                    LENGH
013100040929     C*
013200040929     C* Esegue lancio TISI95R solo x chiusura
013300040929     C                   CLEAR                   TISI95DS
013400040929     C                   EVAL      I95TLA = 'C'
013500040929     C                   CALL      'TISI95R'
013600040929     C                   PARM                    TISI95DS
013700000616     C*
013800010201     C                   seton                                        LR
013900000613
014000000613
014100000613     C*--------------------------------------------------------
014200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014300000613     C*--------------------------------------------------------
014400000621     C     STPR          BEGSR
014500000613     C*
014600040910     C                   if        not %open(titve6p)
014700040910     C                   open      titve6p
014800990915     C                   endif
014900990915     C*
015000990915     C                   except    riepilogo
015100990915     C*
015200040910     C                   if        %open(titve6p)
015300040910     C                   close     titve6p
015400990914     C                   endif
015500990910     C*
015600000613     C                   ENDSR
015700000613     C***
015800990908
015900000801
016000910830     C*--------------------------------------------------------
016100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016200910830     C*--------------------------------------------------------
016300040526     C     RWFILE        BEGSR
016400990910     C*
016500990914     C                   if        not %open(tivin00r)
016600990908     C                   open      tivin00r
016700990914     C                   endif
016800021113     C                   if        not %open(fivabwwr)
016900021113     C                   open      fivabwwr
017000990914     C                   endif
017100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017200020305     C                   exsr      prevat
017300010201     C*
017400010202     C                   if        chkcall = '0'
017500010202     C*
017600021113     C                   if        not %open(fivatwwr)
017700021113     C                   open      fivatwwr
017800010201     C                   endif
017900990910     C*
018000010201     C                   clear                   §CTROKVB          5 0
018100020305     C                   clear                   §CTROKVT          5 0
018200000801     C                   clear                   §CTRMO            5 0
018300000801     C                   clear                   §CTRNO            5 0
018400040910     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800040910     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000000801     C*
019100000801     C                   if        *in70 = *off
019200000801     C                             and
019300000801     C                             (vinflg = *blanks
019400000801     C                              or vinflg = '0'
019500000801     C                              or vinflg = '2')
019600000801     C*
019700000801     C                   clear                   vinmsg
019800000801     C                   eval      vinflg = '1'
019900040910     C*
020000040910     C* Eseguo routine d traduzione
020100040910     C                   exsr      impvabvat
020200040802     C*
020300010305     C                   endif
020400000905     C*
020500000905     C                   else
020600000905     C                   eval      vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900000905     C  N70              update    tivin000
021000000905     C*
021100991022     C  N70              ENDdo
021200010202     C*
021300010202     C                   endif
021400990910
021500990910     C* Se non ci sono record con errori ...
021600000710     C                   if        §ctrno = 0
021700990910     C* ... restituisco esito OK.
021800990921     C                   eval      wrkesito = '0'
021900990910     C                   else
022000010201     C                   if        §ctrokvb > 0
022100990921     C                   eval      wrkesito = '1'
022200000710     C                   else
022300000710     C                   eval      wrkesito = '2'
022400990910     C                   endif
022500000710     C                   endif
022600990910     C*
022700990914     C                   if        %open(tivin00r)
022800990908     C                   close     tivin00r
022900990914     C                   endif
023000021113     C                   if        %open(fivabwwr)
023100021113     C                   close     fivabwwr
023200990914     C                   endif
023300021113     C                   if        %open(fivatwwr)
023400021113     C                   close     fivatwwr
023500010201     C                   endif
023600990910     C*
023700010201     C                   if        §ctrokvb > 0
023800000724     C                             and vlrpoi <> *zeros
023900010202     C                   exsr      invio
024000990920     C                   endif
024100990920     C*
024200910830     C                   ENDSR
024300000613     C***
024400990920
024500000801     C*----------------------------------------------------*
024600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024700000801     C*----------------------------------------------------*
024800010201     C     INZVAR        BEGSR
024900000801     C*
025000040802     C                   Z-ADD     *zeros        Num5_0            5 0
025100040802     C                   MOVEL     '0'           FlgCAS            1
025200000801     C*
025300000801     C                   ENDSR
025400000801     C*----------------------------------------------------*
025500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
025600000801     C*----------------------------------------------------*
025700000801     C     DEFCAM        BEGSR
025800000801     C*
025900021113     C                   CLEAR                   FIVAB000
026000040802     C                   CLEAR                   FIVAT000
026100020619     C* Imposto i valori di default...
026200040930     C                   Z-ADD     0892930       VABCCM
026300040930     C                   Z-ADD     0892930       VATCCM
026400040910     C                   Z-ADD     089           VABLNP
026500040910     C                   Z-ADD     089           VATLNP
026600040714     C                   Z-ADD     000           VABCTR
026700040714     C                   MOVEL     '7Q'          VABCTM
026800040823     C                   MOVEL     '1'           VABCBO
026900060206     C                   MOVEL     'D'           VABTSP
027000020619     C* ... e poi verifico se sono stati passati come parametri
027100020619     C                   IF        vlrppt > *blanks
027200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027400020619     C                   EXSR      CHKNUM
027500020619     C                   IF        PiInt=*on
027600020619     C                   Z-ADD     PiVal         VABCCM
027700020619     C                   Z-ADD     PiVal         VATCCM
027800020619     C                   ENDIF
027900040506     C                   ENDIF
028000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028200020619     C                   EXSR      CHKNUM
028300020619     C                   IF        PiInt=*on
028400020619     C                   Z-ADD     PiVal         VABLNP
028500020619     C                   Z-ADD     PiVal         VATLNP
028600040506     C                   ENDIF
028700020619     C                   ENDIF
028800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABCTR
029300040506     C                   ENDIF
029400020619     C                   ENDIF
029500070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
029600070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
029700070427     C                   ENDIF
029800020619     C                   ENDIF
029900000801     C*
030000000801     C                   ENDSR
030100000801     C*----------------------------------------------------*
030200040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
030300000801     C*----------------------------------------------------*
030400040910     C     IMPVABVAT     BEGSR
030500040910     C*
030600040910     C* Traduzione relativa ai tipi record del file del cliente
030700040910     C*
030800040910     C***
030900040910     C* ...tipo record 11
031000040910     C                   IF        %subst(vindta:1:2) = '11'
031100040910     C* ......inizializzazioni iniziali e formati record file Bartolini
031200040910     C                   EXSR      INZVAR
031300040910     C                   EXSR      DEFCAM
031400071011     C*
031500071011     C* verifico forzatura x CCM
031600071018     C                   IF        %subst(vindta:3:9) = '934397260'             * MILLIPORE
031700071011     C                   EVAL      VABCCM = 1161200
031800071011     C                   EVAL      VATCCM = 1161200
031900071011     C                   ENDIF
032000080206     C***                IF        %subst(vindta:3:9) = '935395440'             * STAEDLER
032100080206     C                   IF        %subst(vindta:3:9) = '935396200'             * STAEDLER
032200071011     C                   EVAL      VABCCM = 1161201
032300071011     C                   EVAL      VATCCM = 1161201
032400071011     C                   ENDIF
032500040928     C*
032600040928     C                   Z-ADD     *zeros        errore            1 0
032700040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
032800040928     C                   MOVEL     datcor        VABAAS
032900040928     C                   MOVEL     datcor        VATAAS
033000040928     C                   MOVE      datcor        VABMGS
033100040928     C                   MOVE(P)   vlrpoi        VABFGS
033200040928     C                   MOVE(P)   vlrpoi        VATFGS
033300040910     C* ......VABNSP/VATNSP
033400041025     C*                  EVAL      PiStr=%trim(%subst(vindta:21:6))
033500041025     C*                  EXSR      CHKNUM
033600041025     C*                  IF        PiInt=*on
033700041025     C*                  Z-ADD     PiVal         VABNSP
033800041025     C*                  Z-ADD     PiVal         VATNSP
033900041025     C*                  ELSE
034000041025     C*                  ADD       1             errore
034100041025     C*                  EVAL      vinmsg = %trimr(vinmsg)
034200041025     C*                            + ' ' + 'VABNSP/VATNSP'
034300041025     C*                  ENDIF
034400041025     C* NSP => Stacco un numeratore da AZNUM
034500041025     C                   clear                   TRUL33DS
034600041025     C                   eval      I33OPE = *zeros
034700041025     C                   eval      I33CNU = 302
034800041025     C                   eval      I33NUM = 1
034900041025     C                   movel     TRUL33DS      KPJBU
035000041025     C                   call      'TRUL33R'
035100041025     C                   parm                    KPJBA
035200041025     C                   movel     KPJBU         TRUL33DS
035300041025     C                   if        O33ERR = *zeros
035400041025     C                   z-add     O33NRF        VABNSP
035500041025     C                   z-add     O33NRF        VATNSP
035600041025     C                   else
035700041025     C                   Z-ADD     1             errore
035800041025     C                   EVAL      vinmsg = %trimr(vinmsg)
035900041025     C                             + ' ' + 'VABNSP VATNSP'
036000041025     C                   endif
036100040910     C* ......VABRSD
036200040910     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
036300060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
036400060928     C     '@':'A'       XLATE     VABRSD        VABRSD
036500060928     C     '§':'S'       XLATE     VABRSD        VABRSD
036600060928     C     '!':' '       XLATE     VABRSD        VABRSD
036700060928     C     ':':' '       XLATE     VABRSD        VABRSD
036800060928     C*
036900040910     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
037000060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
037100060928     C     '@':'A'       XLATE     VABRD2        VABRD2
037200060928     C     '§':'S'       XLATE     VABRD2        VABRD2
037300060928     C     '!':' '       XLATE     VABRD2        VABRD2
037400060928     C     ':':' '       XLATE     VABRD2        VABRD2
037500040910     C*
037600040910     C                   ENDIF
037700040910     C***
037800040910     C* ...tipo record 12
037900040910     C                   IF        %subst(vindta:1:2) = '12'
038000040910     C* ......VABIND
038100040910     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
038200040910     C* ......VABCAD
038300040910     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
038400040910     C                   EXSR      CHKNUM
038500040910     C                   IF        PiInt=*on
038600040910     C                   Z-ADD     PiVal         Num5_0
038700040910     C                   MOVEL(P)  Num5_0        VABCAD
038800040910     C                   ELSE
038900040910     C                   ADD       1             errore
039000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
039100040910     C                             + ' ' + 'VABCAD'
039200040910     C                   ENDIF
039300040910     C* ......VABLOD
039400040910     C                   EVAL      VABLOD=%trim(%subst(vindta:100:25))
039500040910     C* Reperisco la provincia dal CAP e dalla località
039600040910     C                   IF        VABCAD <> *blanks AND
039700040910     C                             VABPRD  = *blanks
039800040910     C                   CLEAR                   TISI95DS
039900040910     C                   EVAL      I95TCN = '3'
040000040910     C                   Z-ADD     datcor        I95DAT
040100040910     C                   EVAL      I95CAP = VABCAD
040200040910     C                   EVAL      I95LOC = VABLOD
040300040910     C                   CALL      'TISI95R'
040400040910     C                   PARM                    TISI95DS
040500040910     C                   EVAL      VABPRD = O95PRV
040600040910     C                   ENDIF
040700040910     C*
040800040910     C                   ENDIF
040900040910     C***
041000040910     C* ...tipo record 13
041100040910     C                   IF        %subst(vindta:1:2) = '13'
041200050719     C                   Z-ADD     *zeros        VABNCL
041300040910     C* ......VABNCL
041400050719     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))             * colli
041500040910     C                   EXSR      CHKNUM
041600040910     C                   IF        PiInt=*on
041700050719     C                   ADD       PiVal         VABNCL
041800040910     C                   ELSE
041900040910     C                   ADD       1             errore
042000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
042100040910     C                             + ' ' + 'VABNCL'
042200040910     C                   ENDIF
042300050719     C*
042400050719     C                   EVAL      PiStr=%trim(%subst(vindta:27:2))             * bancali
042500040910     C                   EXSR      CHKNUM
042600040910     C                   IF        PiInt=*on
042700050719     C                   ADD       PiVal         VABNCL
042800040910     C                   ELSE
042900040910     C                   ADD       1             errore
043000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
043100040910     C                             + ' ' + 'VABNCL'
043200040910     C                   ENDIF
043300040910     C* ......VABPKB
043400040910     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
043500040910     C                   EXSR      CHKNUM
043600040910     C                   IF        PiNum=*on
043700040910     C                   EVAL(H)   VABPKB=PiVal/100                             * gestisco 2 dec.
043800040910     C                   ELSE
043900040910     C                   ADD       1             errore
044000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
044100040910     C                             + ' ' + 'VABPKB'
044200040910     C                   ENDIF
044300040929     C* ......VABCBO
044400040929     C                   IF        %subst(vindta:46:1) = '2'
044500040929     C                   EVAL      VABCBO='2'
044600040929     C                   ELSE
044700040929     C                   EVAL      VABCBO='1'
044800040929     C                   ENDIF
044900040910     C* ......VABDCR
045000040910     C                   IF        %subst(vindta:47:6) <> *zeros
045100040910     C                   EVAL      PiStr='20'+%trim(%subst(vindta:47:6))
045200040910     C                   EXSR      CHKNUM
045300040910     C                   IF        PiInt=*on
045400040910     C                   Z-ADD     PiVal         VABDCR
045500040910     C                   ELSE
045600040910     C                   ADD       1             errore
045700040910     C                   EVAL      vinmsg = %trimr(vinmsg)
045800040910     C                             + ' ' + 'VABDCR'
045900040910     C                   ENDIF
046000040910     C                   ENDIF
046100040910     C* ......VABHCR
046200040910     C                   EVAL      PiStr=%trim(%subst(vindta:53:4))
046300040910     C                   EXSR      CHKNUM
046400040910     C                   IF        PiInt=*on
046500040910     C                   Z-ADD     PiVal         VABHCR
046600040910     C                   ELSE
046700040910     C                   ADD       1             errore
046800040910     C                   EVAL      vinmsg = %trimr(vinmsg)
046900040910     C                             + ' ' + 'VABHCR'
047000040910     C                   ENDIF
047100040910     C* ......VABCAS
047200040910     C                   IF        %subst(vindta:57:9) <> *zeros
047300040910     C                   EVAL      FlgCAS = '1'
047400040910     C                   ENDIF
047500040910     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
047600040910     C                   EXSR      CHKNUM
047700040910     C                   IF        PiNum=*on
047800040910     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
047900040910     C                   ELSE
048000040910     C                   ADD       1             errore
048100040910     C                   EVAL      vinmsg = %trimr(vinmsg)
048200040910     C                             + ' ' + 'VABCAS'
048300040910     C                   ENDIF
048400040910     C* ......VABVCA
048500040910     C                   EVAL      VABVCA=%trim(%subst(vindta:67:3))
048600040910     C* ......VABIAS
048700060126     C*                  EVAL      PiStr=%trim(%subst(vindta:106:9))
048800060126     C*                  EXSR      CHKNUM
048900060126     C*                  IF        PiNum=*on
049000060126     C*                  EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
049100060126     C*                  ELSE
049200060126     C*                  ADD       1             errore
049300060126     C*                  EVAL      vinmsg = %trimr(vinmsg)
049400060126     C*                            + ' ' + 'VABIAS'
049500060126     C*                  ENDIF
049600040910     C* ......VABVAS
049700040910     C                   EVAL      VABVAS=%trim(%subst(vindta:115:3))
049800040910     C*
049900040910     C                   ENDIF
050000040910     C***
050100040910     C* ...tipo record 14
050200040910     C                   IF        %subst(vindta:1:2) = '14'
050300040910     C* ......VATNOT
050400040910     C                   EVAL      VATNOT=%trim(%subst(vindta:29:24))
050500040910     C                   EVAL      VATTRC = 'E'
050600040910     C* x ogni tipo record 14 devo scaricare il buffer del file FIVAT
050700040910     C                   WRITE     FIVAT000
050800040910     C*
050900040910     C                   ENDIF
051000041111     C***
051100041111     C* ...tipo record 22
051200041111     C                   IF        %subst(vindta:1:2) = '22'
051300041111     C* ......VABRMO
051400041111     C                   EVAL      VABRMO=%trim(%subst(vindta:3:30))
051500041111     C                   ENDIF
051600060913     C***
051700060914     C* ...tipo record 25
051800071031     C                   IF        %subst(vindta:1:2) = '25'
051900071031     C*
052000071031     C* Solo x traffico MILLIPORE
052100071031     C                   IF        VABCCM = 1161200
052200071031     C*
052300071031     C                   EVAL      dsRT25 = vindta
052400071119     C* Consegna particolare (solo valori ben determinati)
052500071119     C                   IF        dRT25_TC1 = 'A' or
052600071119     C                             dRT25_TC1 = 'P' or
052700071119     C                             dRT25_TC1 = 'S'
052800071031     C                   EVAL      VABTC1 = dRT25_TC1
052900071119     C                   ENDIF
053000060913     C* Telefono destinatario
053100071031     C                   EVAL      VATNOT = dRT25_VATB
053200071031     C                   EVAL      VATTRC = 'B'
053300071031     C                   WRITE     FIVAT000
053400061013     C* Referente consegna
053500071031     C                   EVAL      VATNOT = dRT25_VATA
053600071031     C                   EVAL      VATTRC = 'A'
053700071031     C                   WRITE     FIVAT000
053800060913     C* Data consegna richiesta
053900071219     C                   EVAL      PiStr=dRT25_DCRYYYY+dRT25_DCRMM+dRT25_DCRDD
054000071219     C                   EXSR      CHKNUM
054100071219     C                   IF        PiInt=*on
054200071219     C                   Z-ADD     PiVal         VABDCR
054300071219     C                   ELSE
054400071219     C                   Z-ADD     *zeros        VABDCR
054500071219     C                   ADD       1             errore
054600071219     C                   EVAL      vinmsg = %trimr(vinmsg)
054700071219     C                             + ' ' + 'VABDCR'
054800071219     C                   ENDIF
054900071031     C*
055000071031     C                   ENDIF
055100071031     C*
055200071031     C                   ENDIF
055300040910     C***
055400040910     C* ...tipo record 41
055500040910     C                   IF        %subst(vindta:1:2) = '41'
055600040910     C* ......VABNAS
055700040910     C                   EVAL      VABNAS=%trim(%subst(vindta:3:126))
055800040910     C*
055900040910     C                   ENDIF
056000040910     C***
056100040910     C* ...tipo record 42
056200040910     C                   IF        %subst(vindta:1:2) = '42'
056300040910     C* ......VABNOT/VABNT2
056400040910     C                   EVAL      VABNOT=%trim(%subst(vindta:3:35))
056500040910     C                   EVAL      VABNT2=%trim(%subst(vindta:3+35:35))
056600040910     C*
056700040910     C                   ENDIF
056800040910     C***
056900040910     C* ...tipo record 87
057000040910     C                   IF        %subst(vindta:1:2) = '87'
057100040910     C* ......VABRMA
057200050701     C                   EVAL      VABRMA=%trim(%subst(vindta:21:6))
057300040910     C* ......VABRMN
057400040910     C                   EVAL      PiStr=%trim(%subst(vindta:27:15))
057500040910     C                   EXSR      CHKNUM
057600040910     C                   IF        PiInt=*on
057700040910     C                   Z-ADD     PiVal         VABRMN
057800040910     C                   ELSE
057900040910     C                   Z-ADD     1             VABRMN
058000040910     C                   ADD       1             errore
058100040910     C                   EVAL      vinmsg = %trimr(vinmsg)
058200040910     C                             + ' ' + 'VABRMN'
058300040910     C                   ENDIF
058400040910     C*
058500040910     C* Il tipo record 87 "chiude" la sequenza tipi record necessari a comporre 1 ns. bolla
058600040910     C* x cui a questo punto devo scaricare il buffer del file FIVAB quindi...
058700040910     C*
058800040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
058900040910     C                   IF        FlgCAS <> '0'
059000040929     C                   IF        VABCBO = '1'
059100040910     C                   EVAL      VABCBO = '4'
059200040910     C                   ELSE
059300040929     C                   EVAL      VABCBO = '6'
059400040910     C                   ENDIF
059500040929     C                   ENDIF
059600040910     C*
059700040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
059800040910     C                   EXSR      CHKIMPDIV
059900040910     C* ...e in fine...
060000040910     C                   WRITE     FIVAB000
060100040910     C*
060200040910     C                   ENDIF
060300040910     C***
060400010202     C*
060500000801     C* Ebbene...
060600000801     C                   ADD       1             §CTRMO
060700010201     C                   IF        errore <> *zeros
060800000801     C                   ADD       1             §CTRNO
060900000801     C                   EVAL      recko = vindta
061000000801     C                   EXCEPT    rigadett
061100000801     C                   EVAL      vinflg = '2'
061200000801     C                   ELSE
061300010201     C                   ADD       1             §CTROKVB
061400000801     C                   ENDIF
061500000801     C*
061600000801     C                   ENDSR
061700010202     C*----------------------------------------------------*
061800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061900010202     C*----------------------------------------------------*
062000020305     C     PREVAT        BEGSR
062100010202     C*
062200021113     C* Compongo il nome del membro da dare al FIVATWWR
062300010202     C                   eval      parmbr = vlrhdl
062400010202     C                   movel     'M'           parmbr
062500060113     C                   eval      parccm = vlrksc
062600010202     C                   eval      paropz = '1'
062700010202     C* Effettuo la chiamata al CLLE preposto
062800040506     C                   call(e)   'TITVVTC'
062900010202     C                   parm                    parccm
063000010202     C                   parm                    parmbr
063100010202     C                   parm                    paropz
063200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063300010202     C                   if        %error
063400010202     C                   movel     '1'           chkcall
063500010202     C                   else
063600010202     C                   movel     '0'           chkcall
063700010202     C                   endif
063800010202     C*
063900010202     C                   ENDSR
064000000801     C*----------------------------------------------------*
064100000801     C*  CONTROLLO NUMERICITA' CAMPI
064200000801     C*----------------------------------------------------*
064300000801     C     CHKNUM        BEGSR
064400000801     C*
064500000801     C                   call(e)   'ISNUMERIC'
064600000801     C                   PARM                    PiStr            30
064700040714     C                   PARM      ','           PiDecChr          1
064800000801     C                   PARM      *ZEROS        PiVal            30 9
064900000801     C                   PARM      '0'           PiInt             1
065000000801     C                   PARM      '0'           PiNum             1
065100000801     C                   IF        %error
065200000801     C                   EVAL      PiInt=*off
065300000801     C                   ENDIF
065400000801     C*
065500000801     C                   ENDSR
065600000801     C***
065700000801
065800011113
065900011113     C*----------------------------------------------------*
066000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066100011113     C*----------------------------------------------------*
066200011113     C     CHKIMPDIV     BEGSR
066300011113     C*
066400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066500011113     C                   Z-ADD     *zeros        wrkDec            9 9
066600011113     C*
066700011113     C* Come prima cosa effettuo considerazioni sulla divisa
066800011113     C                   IF        vabIAS > *zeros
066900011113     C                   IF        vabVAS <> 'EUR'
067000011113     C                   EVAL      vabVAS =  'ITL'
067100011113     C                   ENDIF
067200011113     C                   ENDIF
067300011113     C*
067400011113     C                   IF        vabCAS > *zeros
067500011113     C                   IF        vabVCA <> 'EUR'
067600011113     C                   EVAL      vabVCA =  'ITL'
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C                   IF        vabVMD > *zeros
068100020305     C                   IF        vabVAD <> 'EUR'
068200011113     C                   EVAL      vabVAD =  'ITL'
068300011113     C                   ENDIF
068400011113     C                   ENDIF
068500011113     C*
068600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068700011113     C                   Z-ADD     vabIAS        wrkDec
068800011113     C                   IF        wrkDec > *zeros
068900011113     C                   IF        vabVAS = 'ITL'
069000011113     C                   EVAL      vabIAS = *zeros
069100011113     C                   ENDIF
069200011113     C                   ENDIF
069300011113     C*
069400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069500011113     C                   Z-ADD     vabCAS        wrkDec
069600011113     C                   IF        wrkDec > *zeros
069700011113     C                   IF        vabVCA = 'ITL'
069800011113     C                   EVAL      vabCAS = *zeros
069900011113     C                   ENDIF
070000011113     C                   ENDIF
070100011113     C*
070200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070300011113     C                   Z-ADD     vabVMD        wrkDec
070400011113     C                   IF        wrkDec > *zeros
070500011113     C                   IF        vabVAD = 'ITL'
070600011113     C                   EVAL      vabVMD = *zeros
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C                   ENDSR
071100011113     C***
071200011113
071300011113
071400000801
071500000801
071600990920      /TITLE Invio dei dati al punto operativo.
071700010202     C     invio         BEGSR
071800990920     C*
071900021113     C* 1° invio FIVAT
072000010201     C                   reset                   dscmz
072100010201     C                   move      vlrpoi        cmzdst
072200021113     C                   eval      cmzfld = 'FIVATWWR'
072300010201     C                   eval      cmzmbd = vlrhdl
072400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072500021009     C***                if        prmfir = *blanks
072600021113     C                   eval      cmzfla = 'FIVAT00F'
072700021113     C                   eval      cmzmba = 'FIVAT00F'
072800021009     C***                else
072900021009     C***                eval      cmzfla = prmfir
073000021009     C***                eval      cmzmba = prmfir
073100021009     C***                endif
073200010201     C                   eval      cmznrr = *zeros
073300020305     C                   move      §ctrokvt      cmznrr
073400021018     C                   eval      cmzlba = vlrfl1
073500010201     C                   call(e)   'TIS711C'
073600010201     C                   parm                    dscmz
073700010201     C                   parm      *blanks       esito
073800010205     C                   if        %error
073900010205     C                             or cmzerr = '1'
074000010205     C                             or esito  = '1'
074100010205     C                   eval      wrkesito = '3'
074200010205     C                   else
074300010201     C*
074400021113     C* 2° invio FIVAB
074500010201     C                   reset                   dscmz
074600010201     C                   move      vlrpoi        cmzdst
074700010201     C                   eval      cmzfld = vlrfou
074800010201     C                   eval      cmzmbd = vlrhdl
074900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075000021009     C***                if        prmfir = *blanks
075100021113     C                   eval      cmzfla = 'FIVAB00F'
075200021113     C                   eval      cmzmba = 'FIVAB00F'
075300021009     C***                else
075400021009     C***                eval      cmzfla = prmfir
075500021009     C***                eval      cmzmba = prmfir
075600021009     C***                endif
075700010201     C                   eval      cmznrr = *zeros
075800010201     C                   move      §ctrokvb      cmznrr
075900021018     C                   eval      cmzlba = vlrfl1
076000010201     C                   call(e)   'TIS711C'
076100010201     C                   parm                    dscmz
076200010201     C                   parm      *blanks       esito
076300010201     C                   if        %error
076400010201     C                             or cmzerr = '1'
076500010201     C                             or esito  = '1'
076600010201     C                   eval      wrkesito = '3'
076700010201     C                   endif
076800010205     C                   endif
076900990920     C*
077000000613     C                   ENDSR
077100000613     C***
077200070411
077300071219     C     *pssr         BEGSR
077400070411     C*
077500071219     C                   if        %open(tivin00r)
077600071219     C                   close     tivin00r
077700071219     C                   endif
077800071219     C                   if        %open(fivabwwr)
077900071219     C                   close     fivabwwr
078000071219     C                   endif
078100071219     C                   if        %open(fivatwwr)
078200071219     C                   close     fivatwwr
078300071219     C                   endif
078400070411     C*
078500070411     C* Effettuo la chiamata al CLLE preposto
078600071219     C                   call(e)   'TITVVTC'
078700071219     C                   parm                    parccm
078800071219     C                   parm                    parmbr
078900071219     C                   parm      '2'           paropz
079000071219     C*
079100071219     C                   eval      wrkesito = '2'
079200071219     C*
079300071219     C                   seton                                        LR
079400070411     C*
079500071219     C                   ENDSR     '*CANCL'
079600070411     C***
079700070411
079800990910
079900000613     C     *inzsr        BEGSR
080000990910     C*
080100990910     C     *entry        plist
080200990920     C                   parm                    tivlrds
080300990921     C                   parm      wrkesito      esito
080400000724     C                   parm                    prmlit
080500000710     C                   parm                    prmfir
080600000613     C*
080700000830     C* CALCOLA LA DATA CORRENTE
080800000830     C                   time                    wn14             14 0
080900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
081000000830     C                   z-add     wn8           g08dat
081100000830     C                   z-add     *zeros        g08inv
081200000830     C                   movel     '0'           g08err
081300000830     C                   call      'XSRDA8'
081400000830     C                   parm                    wlbda8
081500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
081600000830     C*
081700000613     C                   ENDSR
081800000613     C***
081900990908
082000040910     Otitve6p   E            riepilogo         2
082100990915     O                                              'Upload via Internet'
082200990915     O                                           +1 'Traduzione TIVIN00R -'
082300021113     O                                           +1 'FIVABWWR/FIVATWWR'
082400010201     O                                           +1 'Report testate bolle'
082500990915     O          E            riepilogo   2
082600990915     O                       wrkdata
082700990915     O                       wrkora              +1
082800990915     O                       procname            +1
082900990915     O          E            riepilogo   2
083000990915     O                                              'Cliente..................:'
083100990915     O                       VABCCM        z     +1
083200990915     O          E            riepilogo   2
083300990920     O                                              'Riferimento Strategi.....:'
083400990920     O                       vlrhdl              +1
083500990915     O          E            riepilogo   2
083600990915     O                                              'Giusti...................:'
083700010201     O                       §CTROKVB      2   +  1
083800990915     O          E            riepilogo   2
083900990915     O                                              'Sbagliati e corretti.....:'
084000971022     O                       §CTRMO        2   +  1
084100990915     O          E            riepilogo   2
084200990915     O                                              'Sbagliati e scartati.....:'
084300971022     O                       §CTRNO        2   +  1
084400000613
084500040910     Otitve6ps  E            testdett          2
084600000613     O                                              'Upload via Internet'
084700000613     O                                           +1 'Traduzione TIVIN00R -'
084800021113     O                                           +1 'FIVABWWR/FIVATWWR'
084900010201     O                                           +1 'Report testate bolle'
085000000616     O          E            testdett    3
085100000613     O                                           +2 'N° rec'
085200000613     O                                           +3 'Anteprima contenuto'
085300000616     O          E            rigadett    2
085400000613     O                       rrnum               +2
085500000621     O                       recko               +3
085600000616     O          E            findett     2
085700000613     O                       wrkdata
085800000613     O                       wrkora              +1
085900000613     O                       procname            +1
086000000616     O          E            findett     2
086100000613     O                                              'Cliente..................:'
086200000613     O                       VABCCM        z     +1
086300000616     O          E            findett     2
086400000613     O                                              'Riferimento Strategi.....:'
086500000613     O                       vlrhdl              +1
086600000616     O          E            findett     2
086700000613     O                                              'Giusti...................:'
086800010201     O                       §CTROKVB      2   +  1
086900000616     O          E            findett     2
087000000613     O                                              'Sbagliati e corretti.....:'
087100000613     O                       §CTRMO        2   +  1
087200000616     O          E            findett     2
087300000613     O                                              'Sbagliati e scartati.....:'
087400000613     O                       §CTRNO        2   +  1
087500000512** CMD - COMANDI CL
087600040910OVRPRTF FILE(TITVE6P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
087700040910OVRPRTF FILE(TITVE6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
087800040910DLTOVR FILE(TITVE6P TITVE6PS) LVL(*)
087900000512
088000000512
