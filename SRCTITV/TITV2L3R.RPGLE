000100120604      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVADWR.
000200990908     H dftactgrp(*yes)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090313     FEDIVABwr  O    E             DISK    usropn
000700090313     FEDIVATwr  O    E             DISK    usropn
000800120704     FEDIVADwr  O    E             DISK    usropn
000900120604
001000990908
001100041210     D*------------
001200041210     D* SCHIERE
001300041210     D*------------
001400050131     D skNAZISO        S              3    DIM(1000)
001500050131     D skNAZBAR        S              3    DIM(1000)
001600000801     D*----------------------------------------------------
001700000801     D* DICHIARAZIOINE VARIABILI DI WRK
001800000801     D*----------------------------------------------------
001900990920     D dscmz         e ds                  inz
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100050131     D tisi95ds      e ds
002200050131     D ds15          e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700990915     D wrkdata         s               d
002800041210     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200080311     D FlgNewBol       s              1    INZ(*blanks)
003300050131     D jNAZ            s              5  0 INZ(*zeros)
003400000830
003500041025     D*------------------
003600041025     D* DS REPERIMENTO NUMERATORE
003700041025     D*------------------
003800050131     D trul33ds      e ds                  inz
003900041025     D*------------------
004000041025     D* DS ARCHITETTURA
004100041025     D*------------------
004200041025     D kpjba         e ds                  inz
004300041025     D*------------------
004400101217
004500990908
004600120611     D*------------------
004700120611     D* DS RIDEFINIZIONE SEGNACOLLO COMPLETO
004800101217     D*------------------
004900120611     D EXPEDITION_REG  DS                  INZ
005000120611     D  EXP_REG_LNP                   3  0
005100120611     D  EXP_REG_LNA                   3  0
005200120611     D  EXP_REG_NRS                   2  0
005300120611     D  EXP_REG_NCD                   7  0
005400120611     D  EXP_REG_ZNC                   2  0
005500120611     D  EXP_REG_CHKD                  1  0
005600010201
005700010201
005800990915     C                   time                    wrkdata
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000613     C*
006200050131     C                   EXSR      CARTAB                                       CARICA TABELLE
006300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006400000613     C*
006500010202     C* Effettuo la chiamata al CLLE preposto
006600120604     C                   call(e)   'TITVEVDTC'
006700010202     C                   parm                    parccm
006800010202     C                   parm                    parmbr
006900010202     C                   parm      '2'           paropz
007000050131     C*
007100050131     C* Effettuo lancio TISI95 solo x chiusura
007200050131     C                   CLEAR                   TISI95DS
007300050131     C                   EVAL      I95TLA = 'C'
007400050131     C                   CALL      'TISI95R'
007500050131     C                   PARM                    TISI95DS
007600000616     C*
007700010201     C                   seton                                        LR
007800000613
007900000613
008000041210
008100041210     C*--------------------------------------------------------
008200050131     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
008300041210     C*--------------------------------------------------------
008400041210     C     CARTAB        BEGSR
008500041210     C*
008600041210     C* TABELLA '15' - NAZIONI
008700041210     C                   clear                   skNAZISO
008800041210     C                   clear                   skNAZBAR
008900041210     C                   eval      tblKUT = 1
009000041210     C                   eval      tblCOD = '15'
009100041210     C     KEYtabP       setll     tabel00f
009200041210     C     KEYtabP       reade     tabel00f
009300041210     C                   dow       not %eof(tabel00f)
009400041210     C                   if        tblFLG = *blanks
009500041210     C                   movel(p)  tblUNI        ds15
009600041210     C                   add       1             jNAZ
009700041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
009800041210     C                   eval      skNAZISO(jNAZ) = §15COD
009900041210     C                   endif
010000041210     C     KEYtabP       reade     tabel00f
010100041210     C                   enddo
010200041210     C*
010300041210     C                   ENDSR
010400041210     C***
010500041210
010600041210
010700041210
010800910830     C*--------------------------------------------------------
010900120604     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR e EDIVADWR  *
011000910830     C*--------------------------------------------------------
011100040526     C     RWFILE        BEGSR
011200990910     C*
011300990914     C                   if        not %open(tivin00r)
011400990908     C                   open      tivin00r
011500990914     C                   endif
011600090313     C                   if        not %open(edivabwr)
011700090313     C                   open      edivabwr
011800990914     C                   endif
011900120604     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR e EDIVADWR
012000120604     C                   exsr      prevtvd
012100010201     C*
012200010202     C                   if        chkcall = '0'
012300010202     C*
012400090313     C                   if        not %open(edivatwr)
012500090313     C                   open      edivatwr
012600010201     C                   endif
012700120704     C                   if        not %open(edivadwr)
012800120704     C                   open      edivadwr
012900120604     C                   endif
013000990910     C*
013100010201     C                   clear                   §CTROKVB          5 0
013200120604     C                   clear                   §CTROKVT          5 0
013300120604     C                   clear                   §CTROKVD          5 0
013400000801     C                   clear                   §CTRMO            5 0
013500000801     C                   clear                   §CTRNO            5 0
013600040910     C*
013700921023     C                   DO        *HIVAL
013800990913     C*
013900990915     C                   READ      tivin00r                               70
014000040910     C                   if        vindta > *blanks
014100000801     C*
014200000801     C                   if        *in70 = *off
014300000801     C                             and
014400000801     C                             (vinflg = *blanks
014500000801     C                              or vinflg = '0'
014600000801     C                              or vinflg = '2')
014700000801     C*
014800000801     C                   clear                   vinmsg
014900000801     C                   eval      vinflg = '1'
015000040910     C*
015100040910     C* Eseguo routine d traduzione
015200120604     C                   exsr      impvbvtvd
015300040802     C*
015400010305     C                   endif
015500000905     C*
015600000905     C                   else
015700000905     C                   eval      vinflg = '1'
015800000905     C                   endif
015900000905     C*
016000000905     C  N70              update    tivin000
016100000905     C*
016200991022     C  N70              ENDdo
016300041210     C*
016400050131     C* Scarico i buffer eventualmente rimasti in sospeso
016500041210     C                   IF        FlgNewBol = '1'
016600120604     C                   EVAL      VABCMR = 'PRIVALIA'
016700090313     C                   EVAL      VABDCM = datcor
016800090313     C                   EVAL      VABCNT = 1
016900090313     C                   WRITE     EDIVAB00
017000041210     C                   ENDIF
017100010202     C*
017200010202     C                   endif
017300990910
017400990910     C* Se non ci sono record con errori ...
017500000710     C                   if        §ctrno = 0
017600990910     C* ... restituisco esito OK.
017700990921     C                   eval      wrkesito = '0'
017800990910     C                   else
017900010201     C                   if        §ctrokvb > 0
018000990921     C                   eval      wrkesito = '1'
018100000710     C                   else
018200000710     C                   eval      wrkesito = '2'
018300990910     C                   endif
018400000710     C                   endif
018500990910     C*
018600990914     C                   if        %open(tivin00r)
018700990908     C                   close     tivin00r
018800990914     C                   endif
018900090313     C                   if        %open(edivabwr)
019000090313     C                   close     edivabwr
019100990914     C                   endif
019200090313     C                   if        %open(edivatwr)
019300090313     C                   close     edivatwr
019400010201     C                   endif
019500120604     C                   if        %open(edivadwr)
019600120604     C                   close     edivadwr
019700120604     C                   endif
019800990910     C*
019900010201     C                   if        §ctrokvb > 0
020000000724     C                             and vlrpoi <> *zeros
020100010202     C                   exsr      invio
020200990920     C                   endif
020300990920     C*
020400910830     C                   ENDSR
020500000613     C***
020600990920
020700000801     C*----------------------------------------------------*
020800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900000801     C*----------------------------------------------------*
021000010201     C     INZVAR        BEGSR
021100000801     C*
021200040802     C                   Z-ADD     *zeros        Num5_0            5 0
021300040802     C                   MOVEL     '0'           FlgCAS            1
021400000801     C*
021500000801     C                   ENDSR
021600000801     C*----------------------------------------------------*
021700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021800000801     C*----------------------------------------------------*
021900000801     C     DEFCAM        BEGSR
022000000801     C*
022100090313     C                   CLEAR                   EDIVAB00
022200090313     C                   CLEAR                   EDIVAT00
022300020619     C* Imposto i valori di default...
022400120604     C                   Z-ADD     0894560       VABCCM
022500120604     C                   Z-ADD     0894560       VATCCM
022600120604     C                   Z-ADD     0894560       VADCCM
022700050217     C                   Z-ADD     089           VABLNP
022800050217     C                   Z-ADD     089           VATLNP
022900120604     C                   Z-ADD     089           VADLNP
023000120604     C                   Z-ADD     000           VABCTR
023100040823     C                   MOVEL     '1'           VABCBO
023200120604     C                   MOVEL     '2D'          VABCTM
023300020619     C* ... e poi verifico se sono stati passati come parametri
023400020619     C                   IF        vlrppt > *blanks
023500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023700020619     C                   EXSR      CHKNUM
023800020619     C                   IF        PiInt=*on
023900020619     C                   Z-ADD     PiVal         VABCCM
024000020619     C                   Z-ADD     PiVal         VATCCM
024100120604     C                   Z-ADD     PiVal         VADCCM
024200020619     C                   ENDIF
024300040506     C                   ENDIF
024400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024600020619     C                   EXSR      CHKNUM
024700020619     C                   IF        PiInt=*on
024800020619     C                   Z-ADD     PiVal         VABLNP
024900020619     C                   Z-ADD     PiVal         VATLNP
025000120604     C                   Z-ADD     PiVal         VADLNP
025100040506     C                   ENDIF
025200020619     C                   ENDIF
025300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025500020619     C                   EXSR      CHKNUM
025600020619     C                   IF        PiInt=*on
025700020619     C                   Z-ADD     PiVal         VABCTR
025800040506     C                   ENDIF
025900020619     C                   ENDIF
026000041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
026100041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
026200041210     C                   ENDIF
026300020619     C                   ENDIF
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700120604     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT e EDIVAD)
026800000801     C*----------------------------------------------------*
026900120604     C     IMPVBVTVD     BEGSR
027000041210     C*
027100041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
027200041210     C                   Z-ADD     *zeros        errore            1 0
027300050317     C********
027400120604     C* Tipo record 'F' (dati relativi alla transazione: sender ID, etc...)
027500050317     C********
027600120604     C                   IF        %trim(%subst(vindta:1:1)) = 'F'
027700050317     C                   ENDIF
027800040910     C*
027900041210     C********
028000120604     C* Tipo record 'H' (dati testata: riferimenti, destinatario, contrassegno, peso, etc...)
028100041210     C********
028200120604     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
028300090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
028400041210     C                   IF        FlgNewBol = '1'
028500120604     C                   EVAL      VABCMR = 'PRIVALIA'
028600090313     C                   EVAL      VABDCM = datcor
028700090313     C                   EVAL      VABCNT = 1
028800090313     C                   WRITE     EDIVAB00
028900050131     C                   EVAL      FlgNewBol = '0'
029000041210     C                   ENDIF
029100041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
029200040910     C                   EXSR      INZVAR
029300040910     C                   EXSR      DEFCAM
029400041210     C                   EVAL      FlgNewBol = '1'
029500040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
029600040928     C                   MOVEL     datcor        VABAAS
029700040928     C                   MOVEL     datcor        VATAAS
029800120604     C                   MOVEL     datcor        VADAAS
029900040928     C                   MOVE      datcor        VABMGS
030000040928     C                   MOVE(P)   vlrpoi        VABFGS
030100040928     C                   MOVE(P)   vlrpoi        VATFGS
030200120604     C                   MOVE(P)   vlrpoi        VADFGS
030300120604     C* ......VABNSP/VATNSP/VADNSP => Stacco un numeratore da AZNUM
030400041210     C                   CLEAR                   TRUL33DS
030500041210     C                   EVAL      I33OPE = *zeros
030600041210     C                   EVAL      I33CNU = 302
030700041210     C                   EVAL      I33NUM = 1
030800041210     C                   MOVEL     TRUL33DS      KPJBU
030900041210     C                   CALL      'TRUL33R'
031000041210     C                   PARM                    KPJBA
031100041210     C                   MOVEL     KPJBU         TRUL33DS
031200041210     C                   IF        O33ERR = *zeros
031300041210     C                   Z-ADD     O33NRF        VABNSP
031400041210     C                   Z-ADD     O33NRF        VATNSP
031500120604     C                   Z-ADD     O33NRF        VADNSP
031600041210     C                   ELSE
031700041210     C                   ADD       1             errore
031800041210     C                   EVAL      vinmsg = %trimr(vinmsg)
031900120604     C                             + ' ' + 'VABNSP VATNSP VADNSP'
032000041210     C                   ENDIF
032100041210     C* ......VABRMN
032200120604     C                   EVAL      PiStr=%trim(%subst(vindta:61:20))
032300041210     C                   EXSR      CHKNUM
032400041210     C                   IF        PiInt=*on
032500041210     C                   Z-ADD     PiVal         VABRMN
032600041210     C                   ELSE
032700041210     C                   Z-ADD     1             VABRMN
032800041210     C                   ADD       1             errore
032900041210     C                   EVAL      vinmsg = %trimr(vinmsg)
033000041210     C                             + ' ' + 'VABRMN'
033100041210     C                   ENDIF
033200041210     C* ......VABRMA
033300120604     C                   EVALR     VABRMA=%trim(%subst(vindta:61:20))
033400050131     C* ......VABRSD
033500120604     C                   EVAL      VABRSD=%trim(%subst(vindta:445:35))
033600050131     C* ......VABIND
033700120604     C                   EVAL      VABIND=%trim(%subst(vindta:480:50))
033800050131     C* ......VABLOD
033900120604     C                   EVAL      VABLOD=%trim(%subst(vindta:530:35))
034000120611     C* ......VABPRD
034100120611     C                   EVAL      VABPRD=%trim(%subst(vindta:585:20))
034200120604     C* ......VABNZD (conversone da formato *ISO a formato "BRT"
034300120604     C                   EVAL      VABNZD=%trim(%subst(vindta:605:20))
034400050131     C                   Z-ADD     1             jNAZ
034500050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
034600050131     C                   IF        %found
034700050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
034800050131     C                   ENDIF
034900050131     C* ......VABCAD
035000120604     C                   EVAL      PiStr=%trim(%subst(vindta:565:20))
035100050131     C                   EXSR      CHKNUM
035200050131     C                   IF        PiInt=*on
035300050131     C                   Z-ADD     PiVal         Num5_0
035400050131     C                   MOVEL(P)  Num5_0        VABCAD
035500050131     C                   ELSE
035600050131     C                   ADD       1             errore
035700050131     C                   EVAL      vinmsg = %trimr(vinmsg)
035800050131     C                             + ' ' + 'VABCAD'
035900050131     C                   ENDIF
036000050131     C* ......VABPRD (reperita mediante TISI95)
036100120604     C                   IF        VABCAD <> *blanks AND
036200120604     C                             VABNZD  = *blanks
036300050131     C                   CLEAR                   TISI95DS
036400050131     C                   EVAL      I95TCN = '3'
036500050131     C                   Z-ADD     datcor        I95DAT
036600050131     C                   EVAL      I95CAP = VABCAD
036700050131     C                   EVAL      I95LOC = VABLOD
036800050131     C                   CALL      'TISI95R'
036900050131     C                   PARM                    TISI95DS
037000050131     C                   EVAL      VABPRD = O95PRV
037100050131     C                   ENDIF
037200101217     C*
037300120611     C* Gestione "Disk B"
037400120626     C***                EVAL      EXPEDITION_REG = %trim(%subst(vindta:161:20))
037500120626     C***                EVAL      VABNRS = EXP_REG_NRS
037600120626     C***                EVAL      VABLNA = EXP_REG_LNA
037700120626     C***                EVAL      VABZNC = EXP_REG_ZNC
037800120611     C*
037900120626     C***                EVAL      VADNRS = EXP_REG_NRS
038000120626     C***                EVAL      VADNCD = EXP_REG_NCD
038100120611     C                   EVAL      VADCMR = 'PRIVALIA'
038200120611     C                   EVAL      VADCNT = 1
038300120604     C                   WRITE     EDIVAD00
038400120604     C                   ADD       1             §CTROKVD
038500120604     C*
038600120604     C* ......VABNCL
038700120604     C                   EVAL      PiStr=%trim(%subst(vindta:181:5))
038800120604     C                   EXSR      CHKNUM
038900120604     C                   IF        PiInt=*on
039000120604     C                   Z-ADD     PiVal         VABNCL
039100120604     C                   ELSE
039200120604     C                   ADD       1             errore
039300120604     C                   EVAL      vinmsg = %trimr(vinmsg)
039400120604     C                             + ' ' + 'VABNCL'
039500120604     C                   ENDIF
039600120604     C* ......VABPKB
039700120604     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
039800120604     C                   EXSR      CHKNUM
039900120604     C                   IF        PiNum=*on
040000120604     C                   Z-ADD     PiVal         VABPKB
040100120604     C                   ELSE
040200120604     C                   ADD       1             errore
040300120604     C                   EVAL      vinmsg = %trimr(vinmsg)
040400120604     C                             + ' ' + 'VABPKB'
040500120604     C                   ENDIF
040600120604     C* ......VABVLB
040700120604     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
040800120604     C                   EXSR      CHKNUM
040900120604     C                   IF        PiNum=*on
041000120604     C                   EVAL      PiVal=PiVal / 1000000
041100120604     C                   Z-ADD     PiVal         VABVLB
041200120604     C                   ELSE
041300120604     C                   ADD       1             errore
041400120604     C                   EVAL      vinmsg = %trimr(vinmsg)
041500120604     C                             + ' ' + 'VABVLB'
041600120604     C                   ENDIF
041700120604     C* ......VABCAS
041800120604     C                   IF        %subst(vindta:210:10) <> *blanks AND
041900120604     C                             %subst(vindta:210:10) <> *zeros  AND
042000120604     C                             %subst(vindta:210:10) <> '000000.000'
042100120604     C                   EVAL      FlgCAS = '1'
042200120604     C                   EVAL      VABVCA=%trim(%subst(vindta:220:5))
042300120604     C                   EVAL      PiStr=%trim(%subst(vindta:210:10))
042400120604     C                   EXSR      CHKNUM
042500120604     C                   IF        PiNum=*on
042600120604     C                   Z-ADD     PiVal         VABCAS
042700120604     C                   ELSE
042800120604     C                   ADD       1             errore
042900120604     C                   EVAL      vinmsg = %trimr(vinmsg)
043000120604     C                             + ' ' + 'VABCAS'
043100120604     C                   ENDIF
043200120604     C                   ENDIF
043300120604     C* ......VABNOT + VABNT2
043400120604     C                   MOVEL     *blanks       wNOTE            70
043500120605     C                   EVAL      wNOTE=%trim(%subst(vindta:655:300))+
043600120605     C                                   %trim(%subst(vindta:955:300))
043700120604     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
043800120604     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
043900120604     C* ......VATNOT_A (tel referente consegna)
044000120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
044100120627     C                             %trim(%subst(vindta:625:15)):15:0):'4'))
044200050131     C                   EVAL      VATTRC = 'A'
044300120604     C                   IF        VATNOT <> *blanks AND
044400120604     C                             VATNOT <> *zeros
044500120604     C                   EVAL      VATCMR = 'PRIVALIA'
044600090318     C                   EVAL      VATCNT = 1
044700090313     C                   WRITE     EDIVAT00
044800120604     C                   ADD       1             §CTROKVT
044900050131     C                   ENDIF
045000120604     C* ......VATNOT_B (tel referente consegna)
045100120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
045200120627     C                             %trim(%subst(vindta:640:15)):15:0):'4'))
045300101217     C                   EVAL      VATTRC = 'B'
045400120604     C                   IF        VATNOT <> *blanks AND
045500120604     C                             VATNOT <> *zeros
045600120604     C                   EVAL      VATCMR = 'PRIVALIA'
045700101217     C                   EVAL      VATCNT = 1
045800101217     C                   WRITE     EDIVAT00
045900120604     C                   ADD       1             §CTROKVT
046000101217     C                   ENDIF
046100050131     C*
046200050131     C                   ENDIF
046300050131     C*
046400050131     C********
046500120604     C* Tipo record 'L' (dati dettaglio: barcode, peso, volume, colli, etc...)
046600050131     C********
046700120604     C                   IF        %trim(%subst(vindta:1:1)) = 'L'
046800041210     C                   ENDIF
046900040910     C*
047000040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
047100040910     C                   IF        FlgCAS <> '0'
047200040929     C                   IF        VABCBO = '1'
047300040910     C                   EVAL      VABCBO = '4'
047400040910     C                   ELSE
047500040929     C                   EVAL      VABCBO = '6'
047600040910     C                   ENDIF
047700040929     C                   ENDIF
047800040910     C*
047900040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048000040910     C                   EXSR      CHKIMPDIV
048100010202     C*
048200000801     C* Ebbene...
048300000801     C                   ADD       1             §CTRMO
048400010201     C                   IF        errore <> *zeros
048500000801     C                   ADD       1             §CTRNO
048600000801     C                   EVAL      vinflg = '2'
048700000801     C                   ELSE
048800010201     C                   ADD       1             §CTROKVB
048900000801     C                   ENDIF
049000000801     C*
049100000801     C                   ENDSR
049200010202     C*----------------------------------------------------*
049300120604     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR e EDIVADWR
049400010202     C*----------------------------------------------------*
049500120604     C     PREVTVD       BEGSR
049600010202     C*
049700120604     C* Compongo il nome del membro da dare al EDIVATWR e EDIVADWR
049800010202     C                   eval      parmbr = vlrhdl
049900010202     C                   movel     'M'           parmbr
050000041210     C                   eval      parccm = vlrksc
050100010202     C                   eval      paropz = '1'
050200010202     C* Effettuo la chiamata al CLLE preposto
050300120604     C                   call(e)   'TITVEVDTC'
050400010202     C                   parm                    parccm
050500010202     C                   parm                    parmbr
050600010202     C                   parm                    paropz
050700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050800010202     C                   if        %error
050900010202     C                   movel     '1'           chkcall
051000010202     C                   else
051100010202     C                   movel     '0'           chkcall
051200010202     C                   endif
051300010202     C*
051400010202     C                   ENDSR
051500000801     C*----------------------------------------------------*
051600000801     C*  CONTROLLO NUMERICITA' CAMPI
051700000801     C*----------------------------------------------------*
051800000801     C     CHKNUM        BEGSR
051900000801     C*
052000000801     C                   call(e)   'ISNUMERIC'
052100000801     C                   PARM                    PiStr            30
052200041210     C                   PARM      '.'           PiDecChr          1
052300000801     C                   PARM      *ZEROS        PiVal            30 9
052400000801     C                   PARM      '0'           PiInt             1
052500000801     C                   PARM      '0'           PiNum             1
052600000801     C                   IF        %error
052700000801     C                   EVAL      PiInt=*off
052800000801     C                   ENDIF
052900000801     C*
053000000801     C                   ENDSR
053100000801     C***
053200000801
053300011113
053400011113     C*----------------------------------------------------*
053500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053600011113     C*----------------------------------------------------*
053700011113     C     CHKIMPDIV     BEGSR
053800011113     C*
053900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054000011113     C                   Z-ADD     *zeros        wrkDec            9 9
054100011113     C*
054200011113     C* Come prima cosa effettuo considerazioni sulla divisa
054300011113     C                   IF        vabIAS > *zeros
054400011113     C                   IF        vabVAS <> 'EUR'
054500011113     C                   EVAL      vabVAS =  'ITL'
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C                   IF        vabCAS > *zeros
055000011113     C                   IF        vabVCA <> 'EUR'
055100011113     C                   EVAL      vabVCA =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   IF        vabVMD > *zeros
055600020305     C                   IF        vabVAD <> 'EUR'
055700011113     C                   EVAL      vabVAD =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056200011113     C                   Z-ADD     vabIAS        wrkDec
056300011113     C                   IF        wrkDec > *zeros
056400011113     C                   IF        vabVAS = 'ITL'
056500011113     C                   EVAL      vabIAS = *zeros
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057000011113     C                   Z-ADD     vabCAS        wrkDec
057100011113     C                   IF        wrkDec > *zeros
057200011113     C                   IF        vabVCA = 'ITL'
057300011113     C                   EVAL      vabCAS = *zeros
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057800011113     C                   Z-ADD     vabVMD        wrkDec
057900011113     C                   IF        wrkDec > *zeros
058000011113     C                   IF        vabVAD = 'ITL'
058100011113     C                   EVAL      vabVMD = *zeros
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C                   ENDSR
058600011113     C***
058700011113
058800011113
058900000801
059000000801
059100990920      /TITLE Invio dei dati al punto operativo.
059200010202     C     invio         BEGSR
059300990920     C*
059400090313     C* 1° invio EDIVAT
059500010201     C                   reset                   dscmz
059600010201     C                   move      vlrpoi        cmzdst
059700090313     C                   eval      cmzfld = 'EDIVATWR'
059800010201     C                   eval      cmzmbd = vlrhdl
059900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060000021009     C***                if        prmfir = *blanks
060100090313     C                   eval      cmzfla = 'EDIVAT0F'
060200090313     C                   eval      cmzmba = 'EDIVAT0F'
060300021009     C***                else
060400021009     C***                eval      cmzfla = prmfir
060500021009     C***                eval      cmzmba = prmfir
060600021009     C***                endif
060700010201     C                   eval      cmznrr = *zeros
060800020305     C                   move      §ctrokvt      cmznrr
060900021018     C                   eval      cmzlba = vlrfl1
061000010201     C                   call(e)   'TIS711C'
061100010201     C                   parm                    dscmz
061200010201     C                   parm      *blanks       esito
061300010205     C                   if        %error
061400010205     C                             or cmzerr = '1'
061500010205     C                             or esito  = '1'
061600010205     C                   eval      wrkesito = '3'
061700010205     C                   else
061800120604     C*
061900120604     C* 2° invio EDIVAD
062000120604     C                   reset                   dscmz
062100120604     C                   move      vlrpoi        cmzdst
062200120604     C                   eval      cmzfld = 'EDIVADWR'
062300120604     C                   eval      cmzmbd = vlrhdl
062400120604     C                   eval      %subst(cmzmbd:1:1) = 'M'
062500120604     C***                if        prmfir = *blanks
062600120604     C                   eval      cmzfla = 'EDIVAD0F'
062700120604     C                   eval      cmzmba = 'EDIVAD0F'
062800120604     C***                else
062900120604     C***                eval      cmzfla = prmfir
063000120604     C***                eval      cmzmba = prmfir
063100120604     C***                endif
063200120604     C                   eval      cmznrr = *zeros
063300120604     C                   move      §ctrokvd      cmznrr
063400120604     C                   eval      cmzlba = vlrfl1
063500120604     C                   call(e)   'TIS711C'
063600120604     C                   parm                    dscmz
063700120604     C                   parm      *blanks       esito
063800120604     C                   if        %error
063900120604     C                             or cmzerr = '1'
064000120604     C                             or esito  = '1'
064100120604     C                   eval      wrkesito = '3'
064200120604     C                   else
064300010201     C*
064400120604     C* 3° invio EDIVAB
064500010201     C                   reset                   dscmz
064600010201     C                   move      vlrpoi        cmzdst
064700010201     C                   eval      cmzfld = vlrfou
064800010201     C                   eval      cmzmbd = vlrhdl
064900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000021009     C***                if        prmfir = *blanks
065100090313     C                   eval      cmzfla = 'EDIVAB0F'
065200090313     C                   eval      cmzmba = 'EDIVAB0F'
065300021009     C***                else
065400021009     C***                eval      cmzfla = prmfir
065500021009     C***                eval      cmzmba = prmfir
065600021009     C***                endif
065700010201     C                   eval      cmznrr = *zeros
065800010201     C                   move      §ctrokvb      cmznrr
065900021018     C                   eval      cmzlba = vlrfl1
066000010201     C                   call(e)   'TIS711C'
066100010201     C                   parm                    dscmz
066200010201     C                   parm      *blanks       esito
066300010201     C                   if        %error
066400010201     C                             or cmzerr = '1'
066500010201     C                             or esito  = '1'
066600010201     C                   eval      wrkesito = '3'
066700010201     C                   endif
066800010205     C                   endif
066900120604     C                   endif
067000990920     C*
067100000613     C                   ENDSR
067200000613     C***
067300120604
067400120604
067500990910
067600000613     C     *inzsr        BEGSR
067700990910     C*
067800990910     C     *entry        plist
067900990920     C                   parm                    tivlrds
068000990921     C                   parm      wrkesito      esito
068100000724     C                   parm                    prmlit
068200000710     C                   parm                    prmfir
068300000613     C*
068400000830     C* CALCOLA LA DATA CORRENTE
068500120604     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068600120604     C                   eval      datcor = %dec(%date() : *ISO)
068700041210     C*
068800041210     C* Chiave su TABEL00F - parziale
068900041210     C     KEYtabP       KLIST
069000041210     C                   KFLD                    tblKUT
069100041210     C                   KFLD                    tblCOD
069200000830     C*
069300000613     C                   ENDSR
069400000613     C***
