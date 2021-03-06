000100121107      /TITLE Upload via Internet: traduzione in EDIVABWR/FIVABWWR/FIVATWWR
000200990908     H dftactgrp(*yes)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600121107     FEDIVABwr  O    E             DISK    usropn
000700121107     FFIVABwwr  O    E             DISK    usropn
000800121107     FFIVATwwr  O    E             DISK    usropn
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
005500120726     D**EXP_REG_CHKD                  1  0
005600120726     D  EXP_REG_TFA                   3  0
005700010201
005800010201
005900990915     C                   time                    wrkdata
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000613     C*
006300050131     C                   EXSR      CARTAB                                       CARICA TABELLE
006400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006500000613     C*
006600010202     C* Effettuo la chiamata al CLLE preposto
006700121107     C                   call(e)   'TITVVTC2'
006800010202     C                   parm                    parccm
006900010202     C                   parm                    parmbr
007000010202     C                   parm      '2'           paropz
007100050131     C*
007200050131     C* Effettuo lancio TISI95 solo x chiusura
007300050131     C                   CLEAR                   TISI95DS
007400050131     C                   EVAL      I95TLA = 'C'
007500050131     C                   CALL      'TISI95R'
007600050131     C                   PARM                    TISI95DS
007700000616     C*
007800010201     C                   seton                                        LR
007900000613
008000000613
008100041210
008200041210     C*--------------------------------------------------------
008300050131     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
008400041210     C*--------------------------------------------------------
008500041210     C     CARTAB        BEGSR
008600041210     C*
008700041210     C* TABELLA '15' - NAZIONI
008800041210     C                   clear                   skNAZISO
008900041210     C                   clear                   skNAZBAR
009000041210     C                   eval      tblKUT = 1
009100041210     C                   eval      tblCOD = '15'
009200041210     C     KEYtabP       setll     tabel00f
009300041210     C     KEYtabP       reade     tabel00f
009400041210     C                   dow       not %eof(tabel00f)
009500041210     C                   if        tblFLG = *blanks
009600041210     C                   movel(p)  tblUNI        ds15
009700041210     C                   add       1             jNAZ
009800041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
009900041210     C                   eval      skNAZISO(jNAZ) = �15COD
010000041210     C                   endif
010100041210     C     KEYtabP       reade     tabel00f
010200041210     C                   enddo
010300041210     C*
010400041210     C                   ENDSR
010500041210     C***
010600041210
010700041210
010800041210
010900910830     C*--------------------------------------------------------
011000121107     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e FIVABWWR e FIVATWWR
011100910830     C*--------------------------------------------------------
011200040526     C     RWFILE        BEGSR
011300990910     C*
011400990914     C                   if        not %open(tivin00r)
011500990908     C                   open      tivin00r
011600990914     C                   endif
011700121107     C                   if        not %open(fivabwwr)
011800121107     C                   open      fivabwwr
011900990914     C                   endif
012000121107     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012100121107     C                   exsr      prevtvb
012200010201     C*
012300010202     C                   if        chkcall = '0'
012400010202     C*
012500121107     C                   if        not %open(edivabwr)
012600121107     C                   open      edivabwr
012700121107     C                   endif
012800121107     C                   if        not %open(fivatwwr)
012900121107     C                   open      fivatwwr
013000010201     C                   endif
013100990910     C*
013200010201     C                   clear                   �CTROKVB          5 0
013300120604     C                   clear                   �CTROKVT          5 0
013400000801     C                   clear                   �CTRMO            5 0
013500000801     C                   clear                   �CTRNO            5 0
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
015200120704     C                   exsr      impvbvt
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
016600121107     C*
016700121107     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
016800121107     C                   IF        VABRMA = 'PRIVALIA_BCN'
016900121107     C                   EVAL      VABCMR = '0896769 PRIVALIA_BCN'
017000121107     C                   EVAL      VABCNT = 1
017100121107     C                   WRITE     EDIVAB00
017200121107     C                   ELSE
017300121107     C                   WRITE     FIVAB000
017400121107     C                   ENDIF
017500041210     C                   ENDIF
017600010202     C*
017700010202     C                   endif
017800990910
017900990910     C* Se non ci sono record con errori ...
018000000710     C                   if        �ctrno = 0
018100990910     C* ... restituisco esito OK.
018200990921     C                   eval      wrkesito = '0'
018300990910     C                   else
018400010201     C                   if        �ctrokvb > 0
018500990921     C                   eval      wrkesito = '1'
018600000710     C                   else
018700000710     C                   eval      wrkesito = '2'
018800990910     C                   endif
018900000710     C                   endif
019000990910     C*
019100990914     C                   if        %open(tivin00r)
019200990908     C                   close     tivin00r
019300990914     C                   endif
019400121107     C                   if        %open(fivabwwr)
019500121107     C                   close     fivabwwr
019600990914     C                   endif
019700121107     C                   if        %open(fivatwwr)
019800121107     C                   close     fivatwwr
019900010201     C                   endif
020000121107     C                   if        %open(edivabwr)
020100121107     C                   close     edivabwr
020200121107     C                   endif
020300990910     C*
020400010201     C                   if        �ctrokvb > 0
020500000724     C                             and vlrpoi <> *zeros
020600010202     C                   exsr      invio
020700990920     C                   endif
020800990920     C*
020900910830     C                   ENDSR
021000000613     C***
021100990920
021200000801     C*----------------------------------------------------*
021300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400000801     C*----------------------------------------------------*
021500010201     C     INZVAR        BEGSR
021600000801     C*
021700040802     C                   Z-ADD     *zeros        Num5_0            5 0
021800040802     C                   MOVEL     '0'           FlgCAS            1
021900000801     C*
022000000801     C                   ENDSR
022100000801     C*----------------------------------------------------*
022200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
022300000801     C*----------------------------------------------------*
022400000801     C     DEFCAM        BEGSR
022500000801     C*
022600121107     C                   CLEAR                   EDIVAB00
022700121107     C                   CLEAR                   FIVAB000
022800121107     C                   CLEAR                   FIVAT000
022900020619     C* Imposto i valori di default...
023000120705     C                   Z-ADD     0896769       VABCCM
023100120705     C                   Z-ADD     0896769       VATCCM
023200050217     C                   Z-ADD     089           VABLNP
023300050217     C                   Z-ADD     089           VATLNP
023400120604     C                   Z-ADD     000           VABCTR
023500040823     C                   MOVEL     '1'           VABCBO
023600120604     C                   MOVEL     '2D'          VABCTM
023700020619     C* ... e poi verifico se sono stati passati come parametri
023800020619     C                   IF        vlrppt > *blanks
023900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
024000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024100020619     C                   EXSR      CHKNUM
024200020619     C                   IF        PiInt=*on
024300020619     C                   Z-ADD     PiVal         VABCCM
024400020619     C                   Z-ADD     PiVal         VATCCM
024500020619     C                   ENDIF
024600040506     C                   ENDIF
024700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024900020619     C                   EXSR      CHKNUM
025000020619     C                   IF        PiInt=*on
025100020619     C                   Z-ADD     PiVal         VABLNP
025200020619     C                   Z-ADD     PiVal         VATLNP
025300040506     C                   ENDIF
025400020619     C                   ENDIF
025500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025700020619     C                   EXSR      CHKNUM
025800020619     C                   IF        PiInt=*on
025900020619     C                   Z-ADD     PiVal         VABCTR
026000040506     C                   ENDIF
026100020619     C                   ENDIF
026200041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
026300041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
026400041210     C                   ENDIF
026500020619     C                   ENDIF
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900121107     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
027000000801     C*----------------------------------------------------*
027100120704     C     IMPVBVT       BEGSR
027200041210     C*
027300041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
027400041210     C                   Z-ADD     *zeros        errore            1 0
027500050317     C********
027600120604     C* Tipo record 'F' (dati relativi alla transazione: sender ID, etc...)
027700050317     C********
027800120604     C                   IF        %trim(%subst(vindta:1:1)) = 'F'
027900050317     C                   ENDIF
028000040910     C*
028100041210     C********
028200120604     C* Tipo record 'H' (dati testata: riferimenti, destinatario, contrassegno, peso, etc...)
028300041210     C********
028400120604     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
028500121107     C* ......se gi� effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
028600041210     C                   IF        FlgNewBol = '1'
028700121107     C*
028800121107     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
028900121107     C                   IF        VABRMA = 'PRIVALIA_BCN'
029000121107     C                   EVAL      VABCMR = '0896769 PRIVALIA_BCN'
029100121107     C                   EVAL      VABCNT = 1
029200121107     C                   WRITE     EDIVAB00
029300121107     C                   ELSE
029400121107     C                   WRITE     FIVAB000
029500121107     C                   ENDIF
029600121107     C*
029700050131     C                   EVAL      FlgNewBol = '0'
029800041210     C                   ENDIF
029900041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
030000040910     C                   EXSR      INZVAR
030100040910     C                   EXSR      DEFCAM
030200041210     C                   EVAL      FlgNewBol = '1'
030300040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030400040928     C                   MOVEL     datcor        VABAAS
030500040928     C                   MOVEL     datcor        VATAAS
030600040928     C                   MOVE      datcor        VABMGS
030700040928     C                   MOVE(P)   vlrpoi        VABFGS
030800040928     C                   MOVE(P)   vlrpoi        VATFGS
030900120704     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
031000120705     C***                CLEAR                   TRUL33DS
031100120705     C***                EVAL      I33OPE = *zeros
031200120705     C***                EVAL      I33CNU = 302
031300120705     C***                EVAL      I33NUM = 1
031400120705     C***                MOVEL     TRUL33DS      KPJBU
031500120705     C***                CALL      'TRUL33R'
031600120705     C***                PARM                    KPJBA
031700120705     C***                MOVEL     KPJBU         TRUL33DS
031800120705     C***                IF        O33ERR = *zeros
031900120705     C***                Z-ADD     O33NRF        VABNSP
032000120705     C***                Z-ADD     O33NRF        VATNSP
032100120705     C***                ELSE
032200120705     C***                ADD       1             errore
032300120705     C***                EVAL      vinmsg = %trimr(vinmsg)
032400120705     C***                          + ' ' + 'VABNSP VATNSP'
032500120705     C***                ENDIF
032600120705     C* ......VABRMN / VABNSP / VATNSP
032700120604     C                   EVAL      PiStr=%trim(%subst(vindta:61:20))
032800041210     C                   EXSR      CHKNUM
032900041210     C                   IF        PiInt=*on
033000041210     C                   Z-ADD     PiVal         VABRMN
033100120705     C                   Z-ADD     PiVal         VABNSP
033200120705     C                   Z-ADD     PiVal         VATNSP
033300041210     C                   ELSE
033400041210     C                   Z-ADD     1             VABRMN
033500041210     C                   ADD       1             errore
033600041210     C                   EVAL      vinmsg = %trimr(vinmsg)
033700120705     C                             + ' ' + 'VABRMN VABNSP VATNSP'
033800041210     C                   ENDIF
033900041210     C* ......VABRMA
034000121008     C***                EVALR     VABRMA=%trim(%subst(vindta:61:20))
034100121008     C                   EVAL      VABRMA=%trim(%subst(vindta:235:35))
034200121008     C* ......VABCTM
034300121008     C                   IF        %trim(%subst(vindta:235:35)) = 'PRIVALIA_BCN'
034400121008     C                   EVAL      VABCTM='7 '
034500121008     C                   ENDIF
034600050131     C* ......VABRSD
034700120604     C                   EVAL      VABRSD=%trim(%subst(vindta:445:35))
034800050131     C* ......VABIND
034900120604     C                   EVAL      VABIND=%trim(%subst(vindta:480:50))
035000050131     C* ......VABLOD
035100120604     C                   EVAL      VABLOD=%trim(%subst(vindta:530:35))
035200120611     C* ......VABPRD
035300120611     C                   EVAL      VABPRD=%trim(%subst(vindta:585:20))
035400120604     C* ......VABNZD (conversone da formato *ISO a formato "BRT"
035500120604     C                   EVAL      VABNZD=%trim(%subst(vindta:605:20))
035600050131     C                   Z-ADD     1             jNAZ
035700050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
035800050131     C                   IF        %found
035900050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
036000050131     C                   ENDIF
036100050131     C* ......VABCAD
036200120604     C                   EVAL      PiStr=%trim(%subst(vindta:565:20))
036300050131     C                   EXSR      CHKNUM
036400050131     C                   IF        PiInt=*on
036500050131     C                   Z-ADD     PiVal         Num5_0
036600050131     C                   MOVEL(P)  Num5_0        VABCAD
036700050131     C                   ELSE
036800050131     C                   ADD       1             errore
036900050131     C                   EVAL      vinmsg = %trimr(vinmsg)
037000050131     C                             + ' ' + 'VABCAD'
037100050131     C                   ENDIF
037200050131     C* ......VABPRD (reperita mediante TISI95)
037300120604     C                   IF        VABCAD <> *blanks AND
037400120604     C                             VABNZD  = *blanks
037500050131     C                   CLEAR                   TISI95DS
037600050131     C                   EVAL      I95TCN = '3'
037700050131     C                   Z-ADD     datcor        I95DAT
037800050131     C                   EVAL      I95CAP = VABCAD
037900050131     C                   EVAL      I95LOC = VABLOD
038000050131     C                   CALL      'TISI95R'
038100050131     C                   PARM                    TISI95DS
038200050131     C                   EVAL      VABPRD = O95PRV
038300050131     C                   ENDIF
038400101217     C*
038500120611     C* Gestione "Disk B"
038600120705     C                   EVAL      EXPEDITION_REG = %trim(%subst(vindta:161:20))
038700120705     C                   EVAL      VABNRS = EXP_REG_NRS
038800120705     C                   EVAL      VATNRS = EXP_REG_NRS
038900120705     C                   EVAL      VABLNA = EXP_REG_LNA
039000120705     C                   EVAL      VABZNC = EXP_REG_ZNC
039100120726 xxx C                   EVAL      VABNAS = %editc(EXP_REG_NCD:'X')+' '+
039200120726 xxx C                                      %editc(EXP_REG_TFA:'X')
039300120604     C*
039400120604     C* ......VABNCL
039500120604     C                   EVAL      PiStr=%trim(%subst(vindta:181:5))
039600120604     C                   EXSR      CHKNUM
039700120604     C                   IF        PiInt=*on
039800120604     C                   Z-ADD     PiVal         VABNCL
039900120604     C                   ELSE
040000120604     C                   ADD       1             errore
040100120604     C                   EVAL      vinmsg = %trimr(vinmsg)
040200120604     C                             + ' ' + 'VABNCL'
040300120604     C                   ENDIF
040400120604     C* ......VABPKB
040500120604     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
040600120604     C                   EXSR      CHKNUM
040700120604     C                   IF        PiNum=*on
040800120604     C                   Z-ADD     PiVal         VABPKB
040900120604     C                   ELSE
041000120604     C                   ADD       1             errore
041100120604     C                   EVAL      vinmsg = %trimr(vinmsg)
041200120604     C                             + ' ' + 'VABPKB'
041300120604     C                   ENDIF
041400120604     C* ......VABVLB
041500120604     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
041600120604     C                   EXSR      CHKNUM
041700120604     C                   IF        PiNum=*on
041800120604     C                   EVAL      PiVal=PiVal / 1000000
041900120604     C                   Z-ADD     PiVal         VABVLB
042000120604     C                   ELSE
042100120604     C                   ADD       1             errore
042200120604     C                   EVAL      vinmsg = %trimr(vinmsg)
042300120604     C                             + ' ' + 'VABVLB'
042400120604     C                   ENDIF
042500120604     C* ......VABCAS
042600120604     C                   IF        %subst(vindta:210:10) <> *blanks AND
042700120604     C                             %subst(vindta:210:10) <> *zeros  AND
042800120604     C                             %subst(vindta:210:10) <> '000000.000'
042900120604     C                   EVAL      FlgCAS = '1'
043000120604     C                   EVAL      VABVCA=%trim(%subst(vindta:220:5))
043100120604     C                   EVAL      PiStr=%trim(%subst(vindta:210:10))
043200120604     C                   EXSR      CHKNUM
043300120604     C                   IF        PiNum=*on
043400120604     C                   Z-ADD     PiVal         VABCAS
043500120604     C                   ELSE
043600120604     C                   ADD       1             errore
043700120604     C                   EVAL      vinmsg = %trimr(vinmsg)
043800120604     C                             + ' ' + 'VABCAS'
043900120604     C                   ENDIF
044000120604     C                   ENDIF
044100120604     C* ......VABNOT + VABNT2
044200120604     C                   MOVEL     *blanks       wNOTE            70
044300120605     C                   EVAL      wNOTE=%trim(%subst(vindta:655:300))+
044400120605     C                                   %trim(%subst(vindta:955:300))
044500120604     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
044600120604     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
044700120604     C* ......VATNOT_A (tel referente consegna)
044800120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
044900120627     C                             %trim(%subst(vindta:625:15)):15:0):'4'))
045000050131     C                   EVAL      VATTRC = 'A'
045100120604     C                   IF        VATNOT <> *blanks AND
045200120604     C                             VATNOT <> *zeros
045300121107     C                   WRITE     FIVAT000
045400120604     C                   ADD       1             �CTROKVT
045500050131     C                   ENDIF
045600120604     C* ......VATNOT_B (tel referente consegna)
045700120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
045800120627     C                             %trim(%subst(vindta:640:15)):15:0):'4'))
045900101217     C                   EVAL      VATTRC = 'B'
046000120604     C                   IF        VATNOT <> *blanks AND
046100120604     C                             VATNOT <> *zeros
046200121107     C                   WRITE     FIVAT000
046300120604     C                   ADD       1             �CTROKVT
046400101217     C                   ENDIF
046500050131     C*
046600050131     C                   ENDIF
046700050131     C*
046800050131     C********
046900120604     C* Tipo record 'L' (dati dettaglio: barcode, peso, volume, colli, etc...)
047000050131     C********
047100120604     C                   IF        %trim(%subst(vindta:1:1)) = 'L'
047200041210     C                   ENDIF
047300040910     C*
047400040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
047500040910     C                   IF        FlgCAS <> '0'
047600040929     C                   IF        VABCBO = '1'
047700040910     C                   EVAL      VABCBO = '4'
047800040910     C                   ELSE
047900040929     C                   EVAL      VABCBO = '6'
048000040910     C                   ENDIF
048100040929     C                   ENDIF
048200040910     C*
048300040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048400040910     C                   EXSR      CHKIMPDIV
048500010202     C*
048600000801     C* Ebbene...
048700000801     C                   ADD       1             �CTRMO
048800010201     C                   IF        errore <> *zeros
048900000801     C                   ADD       1             �CTRNO
049000000801     C                   EVAL      vinflg = '2'
049100000801     C                   ELSE
049200010201     C                   ADD       1             �CTROKVB
049300000801     C                   ENDIF
049400000801     C*
049500000801     C                   ENDSR
049600010202     C*----------------------------------------------------*
049700121107     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049800010202     C*----------------------------------------------------*
049900121107     C     PREVTVB       BEGSR
050000010202     C*
050100121107     C* Compongo il nome del membro da dare al FIVATWWR
050200010202     C                   eval      parmbr = vlrhdl
050300010202     C                   movel     'M'           parmbr
050400041210     C                   eval      parccm = vlrksc
050500010202     C                   eval      paropz = '1'
050600010202     C* Effettuo la chiamata al CLLE preposto
050700121107     C                   call(e)   'TITVVTC2'
050800010202     C                   parm                    parccm
050900010202     C                   parm                    parmbr
051000010202     C                   parm                    paropz
051100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051200010202     C                   if        %error
051300010202     C                   movel     '1'           chkcall
051400010202     C                   else
051500010202     C                   movel     '0'           chkcall
051600010202     C                   endif
051700010202     C*
051800010202     C                   ENDSR
051900000801     C*----------------------------------------------------*
052000000801     C*  CONTROLLO NUMERICITA' CAMPI
052100000801     C*----------------------------------------------------*
052200000801     C     CHKNUM        BEGSR
052300000801     C*
052400000801     C                   call(e)   'ISNUMERIC'
052500000801     C                   PARM                    PiStr            30
052600041210     C                   PARM      '.'           PiDecChr          1
052700000801     C                   PARM      *ZEROS        PiVal            30 9
052800000801     C                   PARM      '0'           PiInt             1
052900000801     C                   PARM      '0'           PiNum             1
053000000801     C                   IF        %error
053100000801     C                   EVAL      PiInt=*off
053200000801     C                   ENDIF
053300000801     C*
053400000801     C                   ENDSR
053500000801     C***
053600000801
053700011113
053800011113     C*----------------------------------------------------*
053900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054000011113     C*----------------------------------------------------*
054100011113     C     CHKIMPDIV     BEGSR
054200011113     C*
054300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilit�
054400011113     C                   Z-ADD     *zeros        wrkDec            9 9
054500011113     C*
054600011113     C* Come prima cosa effettuo considerazioni sulla divisa
054700011113     C                   IF        vabIAS > *zeros
054800011113     C                   IF        vabVAS <> 'EUR'
054900011113     C                   EVAL      vabVAS =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabCAS > *zeros
055400011113     C                   IF        vabVCA <> 'EUR'
055500011113     C                   EVAL      vabVCA =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C                   IF        vabVMD > *zeros
056000020305     C                   IF        vabVAD <> 'EUR'
056100011113     C                   EVAL      vabVAD =  'ITL'
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056600011113     C                   Z-ADD     vabIAS        wrkDec
056700011113     C                   IF        wrkDec > *zeros
056800011113     C                   IF        vabVAS = 'ITL'
056900011113     C                   EVAL      vabIAS = *zeros
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057400011113     C                   Z-ADD     vabCAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVCA = 'ITL'
057700011113     C                   EVAL      vabCAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058200011113     C                   Z-ADD     vabVMD        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVAD = 'ITL'
058500011113     C                   EVAL      vabVMD = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   ENDSR
059000011113     C***
059100011113
059200011113
059300000801
059400000801
059500990920      /TITLE Invio dei dati al punto operativo.
059600010202     C     invio         BEGSR
059700990920     C*
059800121107     C* 1� invio FIVAT
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100121107     C                   eval      cmzfld = 'FIVATWWR'
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500121107     C                   eval      cmzfla = 'FIVAT00F'
060600121107     C                   eval      cmzmba = 'FIVAT00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200020305     C                   move      �ctrokvt      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010205     C                   if        %error
061800010205     C                             or cmzerr = '1'
061900010205     C                             or esito  = '1'
062000010205     C                   eval      wrkesito = '3'
062100121107     C                   endif
062200010201     C*
062300121107     C* 2� invio FIVAB
062400010201     C                   reset                   dscmz
062500010201     C                   move      vlrpoi        cmzdst
062600010201     C                   eval      cmzfld = vlrfou
062700010201     C                   eval      cmzmbd = vlrhdl
062800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062900021009     C***                if        prmfir = *blanks
063000121107     C                   eval      cmzfla = 'FIVAB00F'
063100121107     C                   eval      cmzmba = 'FIVAB00F'
063200021009     C***                else
063300021009     C***                eval      cmzfla = prmfir
063400021009     C***                eval      cmzmba = prmfir
063500021009     C***                endif
063600010201     C                   eval      cmznrr = *zeros
063700010201     C                   move      �ctrokvb      cmznrr
063800021018     C                   eval      cmzlba = vlrfl1
063900010201     C                   call(e)   'TIS711C'
064000010201     C                   parm                    dscmz
064100010201     C                   parm      *blanks       esito
064200010201     C                   if        %error
064300010201     C                             or cmzerr = '1'
064400010201     C                             or esito  = '1'
064500010201     C                   eval      wrkesito = '3'
064600121107     C                   endif
064700121107     C*
064800121107     C* 3� invio EDIVAB
064900121107     C                   reset                   dscmz
065000121107     C                   move      vlrpoi        cmzdst
065100121107     C                   eval      cmzfld = 'EDIVABWR'
065200121107     C                   eval      cmzmbd = vlrhdl
065300121107     C                   eval      %subst(cmzmbd:1:1) = 'M'
065400121107     C***                if        prmfir = *blanks
065500121107     C                   eval      cmzfla = 'EDIVAB0F'
065600121107     C                   eval      cmzmba = 'EDIVAB0F'
065700121107     C***                else
065800121107     C***                eval      cmzfla = prmfir
065900121107     C***                eval      cmzmba = prmfir
066000121107     C***                endif
066100121107     C                   eval      cmznrr = *zeros
066200121107     C                   move      �ctrokvb      cmznrr
066300121107     C                   eval      cmzlba = vlrfl1
066400121107     C                   call(e)   'TIS711C'
066500121107     C                   parm                    dscmz
066600121107     C                   parm      *blanks       esito
066700121107     C                   if        %error
066800121107     C                             or cmzerr = '1'
066900121107     C                             or esito  = '1'
067000121107     C                   eval      wrkesito = '3'
067100010201     C                   endif
067200990920     C*
067300000613     C                   ENDSR
067400000613     C***
067500120604
067600120604
067700990910
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500000613     C*
068600000830     C* CALCOLA LA DATA CORRENTE
068700120604     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068800120604     C                   eval      datcor = %dec(%date() : *ISO)
068900041210     C*
069000041210     C* Chiave su TABEL00F - parziale
069100041210     C     KEYtabP       KLIST
069200041210     C                   KFLD                    tblKUT
069300041210     C                   KFLD                    tblCOD
069400000830     C*
069500000613     C                   ENDSR
069600000613     C***
