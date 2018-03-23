000100120704      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR
000200990908     H dftactgrp(*yes)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090313     FEDIVABwr  O    E             DISK    usropn
000700090313     FEDIVATwr  O    E             DISK    usropn
000800120604
000900990908
001000041210     D*------------
001100041210     D* SCHIERE
001200041210     D*------------
001300050131     D skNAZISO        S              3    DIM(1000)
001400050131     D skNAZBAR        S              3    DIM(1000)
001500000801     D*----------------------------------------------------
001600000801     D* DICHIARAZIOINE VARIABILI DI WRK
001700000801     D*----------------------------------------------------
001800990920     D dscmz         e ds                  inz
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000050131     D tisi95ds      e ds
002100050131     D ds15          e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600990915     D wrkdata         s               d
002700041210     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100080311     D FlgNewBol       s              1    INZ(*blanks)
003200050131     D jNAZ            s              5  0 INZ(*zeros)
003300000830
003400041025     D*------------------
003500041025     D* DS REPERIMENTO NUMERATORE
003600041025     D*------------------
003700050131     D trul33ds      e ds                  inz
003800041025     D*------------------
003900041025     D* DS ARCHITETTURA
004000041025     D*------------------
004100041025     D kpjba         e ds                  inz
004200041025     D*------------------
004300101217
004400990908
004500120611     D*------------------
004600120611     D* DS RIDEFINIZIONE SEGNACOLLO COMPLETO
004700101217     D*------------------
004800120611     D EXPEDITION_REG  DS                  INZ
004900120611     D  EXP_REG_LNP                   3  0
005000120611     D  EXP_REG_LNA                   3  0
005100120611     D  EXP_REG_NRS                   2  0
005200120611     D  EXP_REG_NCD                   7  0
005300120611     D  EXP_REG_ZNC                   2  0
005400120726     D**EXP_REG_CHKD                  1  0
005500120726     D  EXP_REG_TFA                   3  0
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
006600120704     C                   call(e)   'TITVEVTC'
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
010900120704     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR
011000910830     C*--------------------------------------------------------
011100040526     C     RWFILE        BEGSR
011200990910     C*
011300990914     C                   if        not %open(tivin00r)
011400990908     C                   open      tivin00r
011500990914     C                   endif
011600090313     C                   if        not %open(edivabwr)
011700090313     C                   open      edivabwr
011800990914     C                   endif
011900120704     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012000120704     C                   exsr      prevt
012100010201     C*
012200010202     C                   if        chkcall = '0'
012300010202     C*
012400090313     C                   if        not %open(edivatwr)
012500090313     C                   open      edivatwr
012600010201     C                   endif
012700990910     C*
012800010201     C                   clear                   §CTROKVB          5 0
012900120604     C                   clear                   §CTROKVT          5 0
013000000801     C                   clear                   §CTRMO            5 0
013100000801     C                   clear                   §CTRNO            5 0
013200040910     C*
013300921023     C                   DO        *HIVAL
013400990913     C*
013500990915     C                   READ      tivin00r                               70
013600040910     C                   if        vindta > *blanks
013700000801     C*
013800000801     C                   if        *in70 = *off
013900000801     C                             and
014000000801     C                             (vinflg = *blanks
014100000801     C                              or vinflg = '0'
014200000801     C                              or vinflg = '2')
014300000801     C*
014400000801     C                   clear                   vinmsg
014500000801     C                   eval      vinflg = '1'
014600040910     C*
014700040910     C* Eseguo routine d traduzione
014800120704     C                   exsr      impvbvt
014900040802     C*
015000010305     C                   endif
015100000905     C*
015200000905     C                   else
015300000905     C                   eval      vinflg = '1'
015400000905     C                   endif
015500000905     C*
015600000905     C  N70              update    tivin000
015700000905     C*
015800991022     C  N70              ENDdo
015900041210     C*
016000050131     C* Scarico i buffer eventualmente rimasti in sospeso
016100041210     C                   IF        FlgNewBol = '1'
016200121106     C                   EVAL      VABCMR = '0896769'+' '+%trim(VABRMA)+
016300121102     C                                      ' '+%char(%date() : *ISO)
016400090313     C                   EVAL      VABDCM = datcor
016500090313     C                   EVAL      VABCNT = 1
016600090313     C                   WRITE     EDIVAB00
016700041210     C                   ENDIF
016800010202     C*
016900010202     C                   endif
017000990910
017100990910     C* Se non ci sono record con errori ...
017200000710     C                   if        §ctrno = 0
017300990910     C* ... restituisco esito OK.
017400990921     C                   eval      wrkesito = '0'
017500990910     C                   else
017600010201     C                   if        §ctrokvb > 0
017700990921     C                   eval      wrkesito = '1'
017800000710     C                   else
017900000710     C                   eval      wrkesito = '2'
018000990910     C                   endif
018100000710     C                   endif
018200990910     C*
018300990914     C                   if        %open(tivin00r)
018400990908     C                   close     tivin00r
018500990914     C                   endif
018600090313     C                   if        %open(edivabwr)
018700090313     C                   close     edivabwr
018800990914     C                   endif
018900090313     C                   if        %open(edivatwr)
019000090313     C                   close     edivatwr
019100010201     C                   endif
019200990910     C*
019300010201     C                   if        §ctrokvb > 0
019400000724     C                             and vlrpoi <> *zeros
019500010202     C                   exsr      invio
019600990920     C                   endif
019700990920     C*
019800910830     C                   ENDSR
019900000613     C***
020000990920
020100000801     C*----------------------------------------------------*
020200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020300000801     C*----------------------------------------------------*
020400010201     C     INZVAR        BEGSR
020500000801     C*
020600040802     C                   Z-ADD     *zeros        Num5_0            5 0
020700040802     C                   MOVEL     '0'           FlgCAS            1
020800000801     C*
020900000801     C                   ENDSR
021000000801     C*----------------------------------------------------*
021100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021200000801     C*----------------------------------------------------*
021300000801     C     DEFCAM        BEGSR
021400000801     C*
021500090313     C                   CLEAR                   EDIVAB00
021600090313     C                   CLEAR                   EDIVAT00
021700020619     C* Imposto i valori di default...
021800120705     C                   Z-ADD     0896769       VABCCM
021900120705     C                   Z-ADD     0896769       VATCCM
022000050217     C                   Z-ADD     089           VABLNP
022100050217     C                   Z-ADD     089           VATLNP
022200120604     C                   Z-ADD     000           VABCTR
022300040823     C                   MOVEL     '1'           VABCBO
022400120604     C                   MOVEL     '2D'          VABCTM
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
025000041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
025100041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
025200041210     C                   ENDIF
025300020619     C                   ENDIF
025400000801     C*
025500000801     C                   ENDSR
025600000801     C*----------------------------------------------------*
025700120704     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025800000801     C*----------------------------------------------------*
025900120704     C     IMPVBVT       BEGSR
026000041210     C*
026100041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
026200041210     C                   Z-ADD     *zeros        errore            1 0
026300050317     C********
026400120604     C* Tipo record 'F' (dati relativi alla transazione: sender ID, etc...)
026500050317     C********
026600120604     C                   IF        %trim(%subst(vindta:1:1)) = 'F'
026700050317     C                   ENDIF
026800040910     C*
026900041210     C********
027000120604     C* Tipo record 'H' (dati testata: riferimenti, destinatario, contrassegno, peso, etc...)
027100041210     C********
027200120604     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
027300090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
027400041210     C                   IF        FlgNewBol = '1'
027500121106     C                   EVAL      VABCMR = '0896769'+' '+%trim(VABRMA)+
027600121102     C                                      ' '+%char(%date() : *ISO)
027700090313     C                   EVAL      VABDCM = datcor
027800090313     C                   EVAL      VABCNT = 1
027900090313     C                   WRITE     EDIVAB00
028000050131     C                   EVAL      FlgNewBol = '0'
028100041210     C                   ENDIF
028200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
028300040910     C                   EXSR      INZVAR
028400040910     C                   EXSR      DEFCAM
028500041210     C                   EVAL      FlgNewBol = '1'
028600040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
028700040928     C                   MOVEL     datcor        VABAAS
028800040928     C                   MOVEL     datcor        VATAAS
028900040928     C                   MOVE      datcor        VABMGS
029000040928     C                   MOVE(P)   vlrpoi        VABFGS
029100040928     C                   MOVE(P)   vlrpoi        VATFGS
029200120704     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
029300120705     C***                CLEAR                   TRUL33DS
029400120705     C***                EVAL      I33OPE = *zeros
029500120705     C***                EVAL      I33CNU = 302
029600120705     C***                EVAL      I33NUM = 1
029700120705     C***                MOVEL     TRUL33DS      KPJBU
029800120705     C***                CALL      'TRUL33R'
029900120705     C***                PARM                    KPJBA
030000120705     C***                MOVEL     KPJBU         TRUL33DS
030100120705     C***                IF        O33ERR = *zeros
030200120705     C***                Z-ADD     O33NRF        VABNSP
030300120705     C***                Z-ADD     O33NRF        VATNSP
030400120705     C***                ELSE
030500120705     C***                ADD       1             errore
030600120705     C***                EVAL      vinmsg = %trimr(vinmsg)
030700120705     C***                          + ' ' + 'VABNSP VATNSP'
030800120705     C***                ENDIF
030900120705     C* ......VABRMN / VABNSP / VATNSP
031000120604     C                   EVAL      PiStr=%trim(%subst(vindta:61:20))
031100041210     C                   EXSR      CHKNUM
031200041210     C                   IF        PiInt=*on
031300041210     C                   Z-ADD     PiVal         VABRMN
031400120705     C                   Z-ADD     PiVal         VABNSP
031500120705     C                   Z-ADD     PiVal         VATNSP
031600041210     C                   ELSE
031700041210     C                   Z-ADD     1             VABRMN
031800041210     C                   ADD       1             errore
031900041210     C                   EVAL      vinmsg = %trimr(vinmsg)
032000120705     C                             + ' ' + 'VABRMN VABNSP VATNSP'
032100041210     C                   ENDIF
032200041210     C* ......VABRMA
032300121008     C***                EVALR     VABRMA=%trim(%subst(vindta:61:20))
032400121008     C                   EVAL      VABRMA=%trim(%subst(vindta:235:35))
032500121008     C* ......VABCTM
032600121008     C                   IF        %trim(%subst(vindta:235:35)) = 'PRIVALIA_BCN'
032700121008     C                   EVAL      VABCTM='7 '
032800121008     C                   ENDIF
032900050131     C* ......VABRSD
033000120604     C                   EVAL      VABRSD=%trim(%subst(vindta:445:35))
033100050131     C* ......VABIND
033200120604     C                   EVAL      VABIND=%trim(%subst(vindta:480:50))
033300050131     C* ......VABLOD
033400120604     C                   EVAL      VABLOD=%trim(%subst(vindta:530:35))
033500120611     C* ......VABPRD
033600120611     C                   EVAL      VABPRD=%trim(%subst(vindta:585:20))
033700120604     C* ......VABNZD (conversone da formato *ISO a formato "BRT"
033800120604     C                   EVAL      VABNZD=%trim(%subst(vindta:605:20))
033900050131     C                   Z-ADD     1             jNAZ
034000050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
034100050131     C                   IF        %found
034200050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
034300050131     C                   ENDIF
034400050131     C* ......VABCAD
034500120604     C                   EVAL      PiStr=%trim(%subst(vindta:565:20))
034600050131     C                   EXSR      CHKNUM
034700050131     C                   IF        PiInt=*on
034800050131     C                   Z-ADD     PiVal         Num5_0
034900050131     C                   MOVEL(P)  Num5_0        VABCAD
035000050131     C                   ELSE
035100050131     C                   ADD       1             errore
035200050131     C                   EVAL      vinmsg = %trimr(vinmsg)
035300050131     C                             + ' ' + 'VABCAD'
035400050131     C                   ENDIF
035500050131     C* ......VABPRD (reperita mediante TISI95)
035600120604     C                   IF        VABCAD <> *blanks AND
035700120604     C                             VABNZD  = *blanks
035800050131     C                   CLEAR                   TISI95DS
035900050131     C                   EVAL      I95TCN = '3'
036000050131     C                   Z-ADD     datcor        I95DAT
036100050131     C                   EVAL      I95CAP = VABCAD
036200050131     C                   EVAL      I95LOC = VABLOD
036300050131     C                   CALL      'TISI95R'
036400050131     C                   PARM                    TISI95DS
036500050131     C                   EVAL      VABPRD = O95PRV
036600050131     C                   ENDIF
036700101217     C*
036800120611     C* Gestione "Disk B"
036900120705     C                   EVAL      EXPEDITION_REG = %trim(%subst(vindta:161:20))
037000120705     C                   EVAL      VABNRS = EXP_REG_NRS
037100120705     C                   EVAL      VATNRS = EXP_REG_NRS
037200120705     C                   EVAL      VABLNA = EXP_REG_LNA
037300120705     C                   EVAL      VABZNC = EXP_REG_ZNC
037400120726 xxx C                   EVAL      VABNAS = %editc(EXP_REG_NCD:'X')+' '+
037500120726 xxx C                                      %editc(EXP_REG_TFA:'X')
037600120604     C*
037700120604     C* ......VABNCL
037800120604     C                   EVAL      PiStr=%trim(%subst(vindta:181:5))
037900120604     C                   EXSR      CHKNUM
038000120604     C                   IF        PiInt=*on
038100120604     C                   Z-ADD     PiVal         VABNCL
038200120604     C                   ELSE
038300120604     C                   ADD       1             errore
038400120604     C                   EVAL      vinmsg = %trimr(vinmsg)
038500120604     C                             + ' ' + 'VABNCL'
038600120604     C                   ENDIF
038700120604     C* ......VABPKB
038800120604     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
038900120604     C                   EXSR      CHKNUM
039000120604     C                   IF        PiNum=*on
039100120604     C                   Z-ADD     PiVal         VABPKB
039200120604     C                   ELSE
039300120604     C                   ADD       1             errore
039400120604     C                   EVAL      vinmsg = %trimr(vinmsg)
039500120604     C                             + ' ' + 'VABPKB'
039600120604     C                   ENDIF
039700120604     C* ......VABVLB
039800120604     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
039900120604     C                   EXSR      CHKNUM
040000120604     C                   IF        PiNum=*on
040100120604     C                   EVAL      PiVal=PiVal / 1000000
040200120604     C                   Z-ADD     PiVal         VABVLB
040300120604     C                   ELSE
040400120604     C                   ADD       1             errore
040500120604     C                   EVAL      vinmsg = %trimr(vinmsg)
040600120604     C                             + ' ' + 'VABVLB'
040700120604     C                   ENDIF
040800120604     C* ......VABCAS
040900120604     C                   IF        %subst(vindta:210:10) <> *blanks AND
041000120604     C                             %subst(vindta:210:10) <> *zeros  AND
041100120604     C                             %subst(vindta:210:10) <> '000000.000'
041200120604     C                   EVAL      FlgCAS = '1'
041300120604     C                   EVAL      VABVCA=%trim(%subst(vindta:220:5))
041400120604     C                   EVAL      PiStr=%trim(%subst(vindta:210:10))
041500120604     C                   EXSR      CHKNUM
041600120604     C                   IF        PiNum=*on
041700120604     C                   Z-ADD     PiVal         VABCAS
041800120604     C                   ELSE
041900120604     C                   ADD       1             errore
042000120604     C                   EVAL      vinmsg = %trimr(vinmsg)
042100120604     C                             + ' ' + 'VABCAS'
042200120604     C                   ENDIF
042300120604     C                   ENDIF
042400120604     C* ......VABNOT + VABNT2
042500120604     C                   MOVEL     *blanks       wNOTE            70
042600120605     C                   EVAL      wNOTE=%trim(%subst(vindta:655:300))+
042700120605     C                                   %trim(%subst(vindta:955:300))
042800120604     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
042900120604     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
043000120604     C* ......VATNOT_A (tel referente consegna)
043100120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
043200120627     C                             %trim(%subst(vindta:625:15)):15:0):'4'))
043300050131     C                   EVAL      VATTRC = 'A'
043400120604     C                   IF        VATNOT <> *blanks AND
043500120604     C                             VATNOT <> *zeros
043600121106     C                   EVAL      VATCMR = '0896769'+' '+%trim(VABRMA)+
043700121102     C                                      ' '+%char(%date() : *ISO)
043800090318     C                   EVAL      VATCNT = 1
043900090313     C                   WRITE     EDIVAT00
044000120604     C                   ADD       1             §CTROKVT
044100050131     C                   ENDIF
044200120604     C* ......VATNOT_B (tel referente consegna)
044300120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
044400120627     C                             %trim(%subst(vindta:640:15)):15:0):'4'))
044500101217     C                   EVAL      VATTRC = 'B'
044600120604     C                   IF        VATNOT <> *blanks AND
044700120604     C                             VATNOT <> *zeros
044800121106     C                   EVAL      VATCMR = '0896769'+' '+%trim(VABRMA)+
044900121102     C                                      ' '+%char(%date() : *ISO)
045000101217     C                   EVAL      VATCNT = 1
045100101217     C                   WRITE     EDIVAT00
045200120604     C                   ADD       1             §CTROKVT
045300101217     C                   ENDIF
045400050131     C*
045500050131     C                   ENDIF
045600050131     C*
045700050131     C********
045800120604     C* Tipo record 'L' (dati dettaglio: barcode, peso, volume, colli, etc...)
045900050131     C********
046000120604     C                   IF        %trim(%subst(vindta:1:1)) = 'L'
046100041210     C                   ENDIF
046200040910     C*
046300040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046400040910     C                   IF        FlgCAS <> '0'
046500040929     C                   IF        VABCBO = '1'
046600040910     C                   EVAL      VABCBO = '4'
046700040910     C                   ELSE
046800040929     C                   EVAL      VABCBO = '6'
046900040910     C                   ENDIF
047000040929     C                   ENDIF
047100040910     C*
047200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047300040910     C                   EXSR      CHKIMPDIV
047400010202     C*
047500000801     C* Ebbene...
047600000801     C                   ADD       1             §CTRMO
047700010201     C                   IF        errore <> *zeros
047800000801     C                   ADD       1             §CTRNO
047900000801     C                   EVAL      vinflg = '2'
048000000801     C                   ELSE
048100010201     C                   ADD       1             §CTROKVB
048200000801     C                   ENDIF
048300000801     C*
048400000801     C                   ENDSR
048500010202     C*----------------------------------------------------*
048600120704     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048700010202     C*----------------------------------------------------*
048800120704     C     PREVT         BEGSR
048900010202     C*
049000120704     C* Compongo il nome del membro da dare al EDIVATWR
049100010202     C                   eval      parmbr = vlrhdl
049200010202     C                   movel     'M'           parmbr
049300041210     C                   eval      parccm = vlrksc
049400010202     C                   eval      paropz = '1'
049500010202     C* Effettuo la chiamata al CLLE preposto
049600120704     C                   call(e)   'TITVEVTC'
049700010202     C                   parm                    parccm
049800010202     C                   parm                    parmbr
049900010202     C                   parm                    paropz
050000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050100010202     C                   if        %error
050200010202     C                   movel     '1'           chkcall
050300010202     C                   else
050400010202     C                   movel     '0'           chkcall
050500010202     C                   endif
050600010202     C*
050700010202     C                   ENDSR
050800000801     C*----------------------------------------------------*
050900000801     C*  CONTROLLO NUMERICITA' CAMPI
051000000801     C*----------------------------------------------------*
051100000801     C     CHKNUM        BEGSR
051200000801     C*
051300000801     C                   call(e)   'ISNUMERIC'
051400000801     C                   PARM                    PiStr            30
051500041210     C                   PARM      '.'           PiDecChr          1
051600000801     C                   PARM      *ZEROS        PiVal            30 9
051700000801     C                   PARM      '0'           PiInt             1
051800000801     C                   PARM      '0'           PiNum             1
051900000801     C                   IF        %error
052000000801     C                   EVAL      PiInt=*off
052100000801     C                   ENDIF
052200000801     C*
052300000801     C                   ENDSR
052400000801     C***
052500000801
052600011113
052700011113     C*----------------------------------------------------*
052800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052900011113     C*----------------------------------------------------*
053000011113     C     CHKIMPDIV     BEGSR
053100011113     C*
053200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053300011113     C                   Z-ADD     *zeros        wrkDec            9 9
053400011113     C*
053500011113     C* Come prima cosa effettuo considerazioni sulla divisa
053600011113     C                   IF        vabIAS > *zeros
053700011113     C                   IF        vabVAS <> 'EUR'
053800011113     C                   EVAL      vabVAS =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C                   IF        vabCAS > *zeros
054300011113     C                   IF        vabVCA <> 'EUR'
054400011113     C                   EVAL      vabVCA =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C                   IF        vabVMD > *zeros
054900020305     C                   IF        vabVAD <> 'EUR'
055000011113     C                   EVAL      vabVAD =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055500011113     C                   Z-ADD     vabIAS        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVAS = 'ITL'
055800011113     C                   EVAL      vabIAS = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056300011113     C                   Z-ADD     vabCAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVCA = 'ITL'
056600011113     C                   EVAL      vabCAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057100011113     C                   Z-ADD     vabVMD        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVAD = 'ITL'
057400011113     C                   EVAL      vabVMD = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C                   ENDSR
057900011113     C***
058000011113
058100011113
058200000801
058300000801
058400990920      /TITLE Invio dei dati al punto operativo.
058500010202     C     invio         BEGSR
058600990920     C*
058700090313     C* 1° invio EDIVAT
058800010201     C                   reset                   dscmz
058900010201     C                   move      vlrpoi        cmzdst
059000090313     C                   eval      cmzfld = 'EDIVATWR'
059100010201     C                   eval      cmzmbd = vlrhdl
059200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059300021009     C***                if        prmfir = *blanks
059400090313     C                   eval      cmzfla = 'EDIVAT0F'
059500090313     C                   eval      cmzmba = 'EDIVAT0F'
059600021009     C***                else
059700021009     C***                eval      cmzfla = prmfir
059800021009     C***                eval      cmzmba = prmfir
059900021009     C***                endif
060000010201     C                   eval      cmznrr = *zeros
060100020305     C                   move      §ctrokvt      cmznrr
060200021018     C                   eval      cmzlba = vlrfl1
060300010201     C                   call(e)   'TIS711C'
060400010201     C                   parm                    dscmz
060500010201     C                   parm      *blanks       esito
060600010205     C                   if        %error
060700010205     C                             or cmzerr = '1'
060800010205     C                             or esito  = '1'
060900010205     C                   eval      wrkesito = '3'
061000010205     C                   else
061100120604     C*
061200010201     C*
061300120704     C* 2° invio EDIVAB
061400010201     C                   reset                   dscmz
061500010201     C                   move      vlrpoi        cmzdst
061600010201     C                   eval      cmzfld = vlrfou
061700010201     C                   eval      cmzmbd = vlrhdl
061800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061900021009     C***                if        prmfir = *blanks
062000090313     C                   eval      cmzfla = 'EDIVAB0F'
062100090313     C                   eval      cmzmba = 'EDIVAB0F'
062200021009     C***                else
062300021009     C***                eval      cmzfla = prmfir
062400021009     C***                eval      cmzmba = prmfir
062500021009     C***                endif
062600010201     C                   eval      cmznrr = *zeros
062700010201     C                   move      §ctrokvb      cmznrr
062800021018     C                   eval      cmzlba = vlrfl1
062900010201     C                   call(e)   'TIS711C'
063000010201     C                   parm                    dscmz
063100010201     C                   parm      *blanks       esito
063200010201     C                   if        %error
063300010201     C                             or cmzerr = '1'
063400010201     C                             or esito  = '1'
063500010201     C                   eval      wrkesito = '3'
063600010201     C                   endif
063700010205     C                   endif
063800990920     C*
063900000613     C                   ENDSR
064000000613     C***
064100120604
064200120604
064300990910
064400000613     C     *inzsr        BEGSR
064500990910     C*
064600990910     C     *entry        plist
064700990920     C                   parm                    tivlrds
064800990921     C                   parm      wrkesito      esito
064900000724     C                   parm                    prmlit
065000000710     C                   parm                    prmfir
065100000613     C*
065200000830     C* CALCOLA LA DATA CORRENTE
065300120604     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065400120604     C                   eval      datcor = %dec(%date() : *ISO)
065500041210     C*
065600041210     C* Chiave su TABEL00F - parziale
065700041210     C     KEYtabP       KLIST
065800041210     C                   KFLD                    tblKUT
065900041210     C                   KFLD                    tblCOD
066000000830     C*
066100000613     C                   ENDSR
066200000613     C***
