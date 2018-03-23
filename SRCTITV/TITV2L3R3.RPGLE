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
009900041210     C                   eval      skNAZISO(jNAZ) = §15COD
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
013200010201     C                   clear                   §CTROKVB          5 0
013300120604     C                   clear                   §CTROKVT          5 0
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
016600121109     C*
016700121109     C* Verifico se richiesto filtro sul codice cliente mittente *IN50 = PRIVALIA BCN
016800121109     C                   SELECT
016900121109     C*
017000121109     C                   WHEN      wCCM = *blanks                               * TUTTO
017100121107     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
017200121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
017300121120     C                                      wORDER_DATA
017400121109     C   50              EVAL      VABCNT = 1
017500121109     C   50              WRITE     EDIVAB00
017600121109     C  N50              WRITE     FIVAB000
017700121109     C*
017800121109     C                   WHEN      wCCM = 'B'                                   * SOLO PRIVALIA BCN
017900121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
018000121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
018100121120     C                                      wORDER_DATA
018200121109     C   50              EVAL      VABCNT = 1
018300121109     C   50              WRITE     EDIVAB00
018400121109     C*
018500121109     C                   WHEN      wCCM = 'L'
018600121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB                * SOLO LOGHISTES SAP
018700121109     C  N50              WRITE     FIVAB000
018800121109     C*
018900121109     C                   ENDSL
019000121109     C                   ENDIF
019100121109     C*
019200010202     C                   endif
019300990910
019400990910     C* Se non ci sono record con errori ...
019500000710     C                   if        §ctrno = 0
019600990910     C* ... restituisco esito OK.
019700990921     C                   eval      wrkesito = '0'
019800990910     C                   else
019900010201     C                   if        §ctrokvb > 0
020000990921     C                   eval      wrkesito = '1'
020100000710     C                   else
020200000710     C                   eval      wrkesito = '2'
020300990910     C                   endif
020400000710     C                   endif
020500990910     C*
020600990914     C                   if        %open(tivin00r)
020700990908     C                   close     tivin00r
020800990914     C                   endif
020900121107     C                   if        %open(fivabwwr)
021000121107     C                   close     fivabwwr
021100990914     C                   endif
021200121107     C                   if        %open(fivatwwr)
021300121107     C                   close     fivatwwr
021400010201     C                   endif
021500121107     C                   if        %open(edivabwr)
021600121107     C                   close     edivabwr
021700121107     C                   endif
021800990910     C*
021900010201     C                   if        §ctrokvb > 0
022000000724     C                             and vlrpoi <> *zeros
022100010202     C                   exsr      invio
022200990920     C                   endif
022300990920     C*
022400910830     C                   ENDSR
022500000613     C***
022600990920
022700000801     C*----------------------------------------------------*
022800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022900000801     C*----------------------------------------------------*
023000010201     C     INZVAR        BEGSR
023100000801     C*
023200040802     C                   Z-ADD     *zeros        Num5_0            5 0
023300040802     C                   MOVEL     '0'           FlgCAS            1
023400000801     C*
023500000801     C                   ENDSR
023600000801     C*----------------------------------------------------*
023700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023800000801     C*----------------------------------------------------*
023900000801     C     DEFCAM        BEGSR
024000000801     C*
024100121107     C                   CLEAR                   EDIVAB00
024200121107     C                   CLEAR                   FIVAB000
024300121107     C                   CLEAR                   FIVAT000
024400020619     C* Imposto i valori di default...
024500120705     C                   Z-ADD     0896769       VABCCM
024600120705     C                   Z-ADD     0896769       VATCCM
024700050217     C                   Z-ADD     089           VABLNP
024800050217     C                   Z-ADD     089           VATLNP
024900120604     C                   Z-ADD     000           VABCTR
025000040823     C                   MOVEL     '1'           VABCBO
025100120604     C                   MOVEL     '2D'          VABCTM
025200020619     C* ... e poi verifico se sono stati passati come parametri
025300020619     C                   IF        vlrppt > *blanks
025400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025600020619     C                   EXSR      CHKNUM
025700020619     C                   IF        PiInt=*on
025800020619     C                   Z-ADD     PiVal         VABCCM
025900020619     C                   Z-ADD     PiVal         VATCCM
026000020619     C                   ENDIF
026100040506     C                   ENDIF
026200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026400020619     C                   EXSR      CHKNUM
026500020619     C                   IF        PiInt=*on
026600020619     C                   Z-ADD     PiVal         VABLNP
026700020619     C                   Z-ADD     PiVal         VATLNP
026800040506     C                   ENDIF
026900020619     C                   ENDIF
027000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027200020619     C                   EXSR      CHKNUM
027300020619     C                   IF        PiInt=*on
027400020619     C                   Z-ADD     PiVal         VABCTR
027500040506     C                   ENDIF
027600020619     C                   ENDIF
027700041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
027800041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
027900041210     C                   ENDIF
028000121109     C                   MOVEL     *blanks       wCCM              1
028100121109     C                   IF        %subst(vlrppt:16:1) <> *blanks
028200121109     C                   EVAL      wCCM = %subst(vlrppt:16:1)
028300121109     C                   ENDIF
028400020619     C                   ENDIF
028500000801     C*
028600000801     C                   ENDSR
028700000801     C*----------------------------------------------------*
028800121107     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
028900000801     C*----------------------------------------------------*
029000120704     C     IMPVBVT       BEGSR
029100041210     C*
029200041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
029300041210     C                   Z-ADD     *zeros        errore            1 0
029400050317     C********
029500120604     C* Tipo record 'F' (dati relativi alla transazione: sender ID, etc...)
029600050317     C********
029700120604     C                   IF        %trim(%subst(vindta:1:1)) = 'F'
029800050317     C                   ENDIF
029900040910     C*
030000041210     C********
030100120604     C* Tipo record 'H' (dati testata: riferimenti, destinatario, contrassegno, peso, etc...)
030200041210     C********
030300120604     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
030400121107     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
030500041210     C                   IF        FlgNewBol = '1'
030600121109     C*
030700121109     C* Verifico se richiesto filtro sul codice cliente mittente *IN50 = PRIVALIA BCN
030800121109     C                   SELECT
030900121109     C*
031000121109     C                   WHEN      wCCM = *blanks                               * TUTTO
031100121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
031200121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
031300121120     C                                      wORDER_DATA
031400121109     C   50              EVAL      VABCNT = 1
031500121109     C   50              WRITE     EDIVAB00
031600121109     C  N50              WRITE     FIVAB000
031700121109     C*
031800121109     C                   WHEN      wCCM = 'B'                                   * SOLO PRIVALIA BCN
031900121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
032000121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
032100121120     C                                      wORDER_DATA
032200121109     C   50              EVAL      VABCNT = 1
032300121109     C   50              WRITE     EDIVAB00
032400121109     C*
032500121109     C                   WHEN      wCCM = 'L'
032600121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB                * SOLO LOGHISTES SAP
032700121109     C  N50              WRITE     FIVAB000
032800121109     C*
032900121109     C                   ENDSL
033000121109     C*
033100050131     C                   EVAL      FlgNewBol = '0'
033200041210     C                   ENDIF
033300041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
033400040910     C                   EXSR      INZVAR
033500040910     C                   EXSR      DEFCAM
033600041210     C                   EVAL      FlgNewBol = '1'
033700040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
033800040928     C                   MOVEL     datcor        VABAAS
033900040928     C                   MOVEL     datcor        VATAAS
034000040928     C                   MOVE      datcor        VABMGS
034100040928     C                   MOVE(P)   vlrpoi        VABFGS
034200040928     C                   MOVE(P)   vlrpoi        VATFGS
034300120704     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
034400120705     C***                CLEAR                   TRUL33DS
034500120705     C***                EVAL      I33OPE = *zeros
034600120705     C***                EVAL      I33CNU = 302
034700120705     C***                EVAL      I33NUM = 1
034800120705     C***                MOVEL     TRUL33DS      KPJBU
034900120705     C***                CALL      'TRUL33R'
035000120705     C***                PARM                    KPJBA
035100120705     C***                MOVEL     KPJBU         TRUL33DS
035200120705     C***                IF        O33ERR = *zeros
035300120705     C***                Z-ADD     O33NRF        VABNSP
035400120705     C***                Z-ADD     O33NRF        VATNSP
035500120705     C***                ELSE
035600120705     C***                ADD       1             errore
035700120705     C***                EVAL      vinmsg = %trimr(vinmsg)
035800120705     C***                          + ' ' + 'VABNSP VATNSP'
035900120705     C***                ENDIF
036000120705     C* ......VABRMN / VABNSP / VATNSP
036100120604     C                   EVAL      PiStr=%trim(%subst(vindta:61:20))
036200041210     C                   EXSR      CHKNUM
036300041210     C                   IF        PiInt=*on
036400041210     C                   Z-ADD     PiVal         VABRMN
036500120705     C                   Z-ADD     PiVal         VABNSP
036600120705     C                   Z-ADD     PiVal         VATNSP
036700041210     C                   ELSE
036800041210     C                   Z-ADD     1             VABRMN
036900041210     C                   ADD       1             errore
037000041210     C                   EVAL      vinmsg = %trimr(vinmsg)
037100120705     C                             + ' ' + 'VABRMN VABNSP VATNSP'
037200041210     C                   ENDIF
037300041210     C* ......VABRMA
037400121008     C***                EVALR     VABRMA=%trim(%subst(vindta:61:20))
037500121008     C                   EVAL      VABRMA=%trim(%subst(vindta:235:35))
037600121109     C                   SETOFF                                       50
037700121109     C                   IF        %trim(%subst(vindta:235:35)) = 'PRIVALIA_BCN'
037800121109     C                   SETON                                        50
037900121109     C                   ENDIF
038000121120     C* ......VABCMR
038100121120     C                   MOVEL     *blanks       wORDER_DATA       8
038200121120     C                   EVAL      wORDER_DATA = %trim(%subst(vindta:32:8))
038300121008     C* ......VABCTM
038400121109     C   50              EVAL      VABCTM='7 '
038500050131     C* ......VABRSD
038600120604     C                   EVAL      VABRSD=%trim(%subst(vindta:445:35))
038700050131     C* ......VABIND
038800120604     C                   EVAL      VABIND=%trim(%subst(vindta:480:50))
038900050131     C* ......VABLOD
039000120604     C                   EVAL      VABLOD=%trim(%subst(vindta:530:35))
039100120611     C* ......VABPRD
039200120611     C                   EVAL      VABPRD=%trim(%subst(vindta:585:20))
039300120604     C* ......VABNZD (conversone da formato *ISO a formato "BRT"
039400120604     C                   EVAL      VABNZD=%trim(%subst(vindta:605:20))
039500050131     C                   Z-ADD     1             jNAZ
039600050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
039700050131     C                   IF        %found
039800050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
039900050131     C                   ENDIF
040000050131     C* ......VABCAD
040100120604     C                   EVAL      PiStr=%trim(%subst(vindta:565:20))
040200050131     C                   EXSR      CHKNUM
040300050131     C                   IF        PiInt=*on
040400050131     C                   Z-ADD     PiVal         Num5_0
040500050131     C                   MOVEL(P)  Num5_0        VABCAD
040600050131     C                   ELSE
040700050131     C                   ADD       1             errore
040800050131     C                   EVAL      vinmsg = %trimr(vinmsg)
040900050131     C                             + ' ' + 'VABCAD'
041000050131     C                   ENDIF
041100050131     C* ......VABPRD (reperita mediante TISI95)
041200120604     C                   IF        VABCAD <> *blanks AND
041300120604     C                             VABNZD  = *blanks
041400050131     C                   CLEAR                   TISI95DS
041500050131     C                   EVAL      I95TCN = '3'
041600050131     C                   Z-ADD     datcor        I95DAT
041700050131     C                   EVAL      I95CAP = VABCAD
041800050131     C                   EVAL      I95LOC = VABLOD
041900050131     C                   CALL      'TISI95R'
042000050131     C                   PARM                    TISI95DS
042100050131     C                   EVAL      VABPRD = O95PRV
042200050131     C                   ENDIF
042300101217     C*
042400120611     C* Gestione "Disk B"
042500120705     C                   EVAL      EXPEDITION_REG = %trim(%subst(vindta:161:20))
042600120705     C                   EVAL      VABNRS = EXP_REG_NRS
042700120705     C                   EVAL      VATNRS = EXP_REG_NRS
042800120705     C                   EVAL      VABLNA = EXP_REG_LNA
042900120705     C                   EVAL      VABZNC = EXP_REG_ZNC
043000120726 xxx C                   EVAL      VABNAS = %editc(EXP_REG_NCD:'X')+' '+
043100120726 xxx C                                      %editc(EXP_REG_TFA:'X')
043200120604     C*
043300120604     C* ......VABNCL
043400120604     C                   EVAL      PiStr=%trim(%subst(vindta:181:5))
043500120604     C                   EXSR      CHKNUM
043600120604     C                   IF        PiInt=*on
043700120604     C                   Z-ADD     PiVal         VABNCL
043800120604     C                   ELSE
043900120604     C                   ADD       1             errore
044000120604     C                   EVAL      vinmsg = %trimr(vinmsg)
044100120604     C                             + ' ' + 'VABNCL'
044200120604     C                   ENDIF
044300120604     C* ......VABPKB
044400120604     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
044500120604     C                   EXSR      CHKNUM
044600120604     C                   IF        PiNum=*on
044700120604     C                   Z-ADD     PiVal         VABPKB
044800120604     C                   ELSE
044900120604     C                   ADD       1             errore
045000120604     C                   EVAL      vinmsg = %trimr(vinmsg)
045100120604     C                             + ' ' + 'VABPKB'
045200120604     C                   ENDIF
045300120604     C* ......VABVLB
045400120604     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
045500120604     C                   EXSR      CHKNUM
045600120604     C                   IF        PiNum=*on
045700120604     C                   EVAL      PiVal=PiVal / 1000000
045800120604     C                   Z-ADD     PiVal         VABVLB
045900120604     C                   ELSE
046000120604     C                   ADD       1             errore
046100120604     C                   EVAL      vinmsg = %trimr(vinmsg)
046200120604     C                             + ' ' + 'VABVLB'
046300120604     C                   ENDIF
046400120604     C* ......VABCAS
046500120604     C                   IF        %subst(vindta:210:10) <> *blanks AND
046600120604     C                             %subst(vindta:210:10) <> *zeros  AND
046700120604     C                             %subst(vindta:210:10) <> '000000.000'
046800120604     C                   EVAL      FlgCAS = '1'
046900120604     C                   EVAL      VABVCA=%trim(%subst(vindta:220:5))
047000120604     C                   EVAL      PiStr=%trim(%subst(vindta:210:10))
047100120604     C                   EXSR      CHKNUM
047200120604     C                   IF        PiNum=*on
047300120604     C                   Z-ADD     PiVal         VABCAS
047400120604     C                   ELSE
047500120604     C                   ADD       1             errore
047600120604     C                   EVAL      vinmsg = %trimr(vinmsg)
047700120604     C                             + ' ' + 'VABCAS'
047800120604     C                   ENDIF
047900120604     C                   ENDIF
048000120604     C* ......VABNOT + VABNT2
048100120604     C                   MOVEL     *blanks       wNOTE            70
048200120605     C                   EVAL      wNOTE=%trim(%subst(vindta:655:300))+
048300120605     C                                   %trim(%subst(vindta:955:300))
048400120604     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
048500120604     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
048600120604     C* ......VATNOT_A (tel referente consegna)
048700120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
048800120627     C                             %trim(%subst(vindta:625:15)):15:0):'4'))
048900050131     C                   EVAL      VATTRC = 'A'
049000120604     C                   IF        VATNOT <> *blanks AND
049100120604     C                             VATNOT <> *zeros
049200121107     C                   WRITE     FIVAT000
049300120604     C                   ADD       1             §CTROKVT
049400050131     C                   ENDIF
049500120604     C* ......VATNOT_B (tel referente consegna)
049600120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
049700120627     C                             %trim(%subst(vindta:640:15)):15:0):'4'))
049800101217     C                   EVAL      VATTRC = 'B'
049900120604     C                   IF        VATNOT <> *blanks AND
050000120604     C                             VATNOT <> *zeros
050100121107     C                   WRITE     FIVAT000
050200120604     C                   ADD       1             §CTROKVT
050300101217     C                   ENDIF
050400050131     C*
050500050131     C                   ENDIF
050600050131     C*
050700050131     C********
050800120604     C* Tipo record 'L' (dati dettaglio: barcode, peso, volume, colli, etc...)
050900050131     C********
051000120604     C                   IF        %trim(%subst(vindta:1:1)) = 'L'
051100041210     C                   ENDIF
051200040910     C*
051300040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051400040910     C                   IF        FlgCAS <> '0'
051500040929     C                   IF        VABCBO = '1'
051600040910     C                   EVAL      VABCBO = '4'
051700040910     C                   ELSE
051800040929     C                   EVAL      VABCBO = '6'
051900040910     C                   ENDIF
052000040929     C                   ENDIF
052100040910     C*
052200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052300040910     C                   EXSR      CHKIMPDIV
052400010202     C*
052500000801     C* Ebbene...
052600000801     C                   ADD       1             §CTRMO
052700010201     C                   IF        errore <> *zeros
052800000801     C                   ADD       1             §CTRNO
052900000801     C                   EVAL      vinflg = '2'
053000000801     C                   ELSE
053100010201     C                   ADD       1             §CTROKVB
053200000801     C                   ENDIF
053300000801     C*
053400000801     C                   ENDSR
053500010202     C*----------------------------------------------------*
053600121107     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053700010202     C*----------------------------------------------------*
053800121107     C     PREVTVB       BEGSR
053900010202     C*
054000121107     C* Compongo il nome del membro da dare al FIVATWWR
054100010202     C                   eval      parmbr = vlrhdl
054200010202     C                   movel     'M'           parmbr
054300041210     C                   eval      parccm = vlrksc
054400010202     C                   eval      paropz = '1'
054500010202     C* Effettuo la chiamata al CLLE preposto
054600121107     C                   call(e)   'TITVVTC2'
054700010202     C                   parm                    parccm
054800010202     C                   parm                    parmbr
054900010202     C                   parm                    paropz
055000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055100010202     C                   if        %error
055200010202     C                   movel     '1'           chkcall
055300010202     C                   else
055400010202     C                   movel     '0'           chkcall
055500010202     C                   endif
055600010202     C*
055700010202     C                   ENDSR
055800000801     C*----------------------------------------------------*
055900000801     C*  CONTROLLO NUMERICITA' CAMPI
056000000801     C*----------------------------------------------------*
056100000801     C     CHKNUM        BEGSR
056200000801     C*
056300000801     C                   call(e)   'ISNUMERIC'
056400000801     C                   PARM                    PiStr            30
056500041210     C                   PARM      '.'           PiDecChr          1
056600000801     C                   PARM      *ZEROS        PiVal            30 9
056700000801     C                   PARM      '0'           PiInt             1
056800000801     C                   PARM      '0'           PiNum             1
056900000801     C                   IF        %error
057000000801     C                   EVAL      PiInt=*off
057100000801     C                   ENDIF
057200000801     C*
057300000801     C                   ENDSR
057400000801     C***
057500000801
057600011113
057700011113     C*----------------------------------------------------*
057800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057900011113     C*----------------------------------------------------*
058000011113     C     CHKIMPDIV     BEGSR
058100011113     C*
058200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058300011113     C                   Z-ADD     *zeros        wrkDec            9 9
058400011113     C*
058500011113     C* Come prima cosa effettuo considerazioni sulla divisa
058600011113     C                   IF        vabIAS > *zeros
058700011113     C                   IF        vabVAS <> 'EUR'
058800011113     C                   EVAL      vabVAS =  'ITL'
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C                   IF        vabCAS > *zeros
059300011113     C                   IF        vabVCA <> 'EUR'
059400011113     C                   EVAL      vabVCA =  'ITL'
059500011113     C                   ENDIF
059600011113     C                   ENDIF
059700011113     C*
059800011113     C                   IF        vabVMD > *zeros
059900020305     C                   IF        vabVAD <> 'EUR'
060000011113     C                   EVAL      vabVAD =  'ITL'
060100011113     C                   ENDIF
060200011113     C                   ENDIF
060300011113     C*
060400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060500011113     C                   Z-ADD     vabIAS        wrkDec
060600011113     C                   IF        wrkDec > *zeros
060700011113     C                   IF        vabVAS = 'ITL'
060800011113     C                   EVAL      vabIAS = *zeros
060900011113     C                   ENDIF
061000011113     C                   ENDIF
061100011113     C*
061200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061300011113     C                   Z-ADD     vabCAS        wrkDec
061400011113     C                   IF        wrkDec > *zeros
061500011113     C                   IF        vabVCA = 'ITL'
061600011113     C                   EVAL      vabCAS = *zeros
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062100011113     C                   Z-ADD     vabVMD        wrkDec
062200011113     C                   IF        wrkDec > *zeros
062300011113     C                   IF        vabVAD = 'ITL'
062400011113     C                   EVAL      vabVMD = *zeros
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C                   ENDSR
062900011113     C***
063000011113
063100011113
063200000801
063300000801
063400990920      /TITLE Invio dei dati al punto operativo.
063500010202     C     invio         BEGSR
063600990920     C*
063700121107     C* 1° invio FIVAT
063800010201     C                   reset                   dscmz
063900010201     C                   move      vlrpoi        cmzdst
064000121107     C                   eval      cmzfld = 'FIVATWWR'
064100010201     C                   eval      cmzmbd = vlrhdl
064200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064300021009     C***                if        prmfir = *blanks
064400121107     C                   eval      cmzfla = 'FIVAT00F'
064500121107     C                   eval      cmzmba = 'FIVAT00F'
064600021009     C***                else
064700021009     C***                eval      cmzfla = prmfir
064800021009     C***                eval      cmzmba = prmfir
064900021009     C***                endif
065000010201     C                   eval      cmznrr = *zeros
065100020305     C                   move      §ctrokvt      cmznrr
065200021018     C                   eval      cmzlba = vlrfl1
065300010201     C                   call(e)   'TIS711C'
065400010201     C                   parm                    dscmz
065500010201     C                   parm      *blanks       esito
065600010205     C                   if        %error
065700010205     C                             or cmzerr = '1'
065800010205     C                             or esito  = '1'
065900010205     C                   eval      wrkesito = '3'
066000121107     C                   endif
066100010201     C*
066200121107     C* 2° invio FIVAB
066300010201     C                   reset                   dscmz
066400010201     C                   move      vlrpoi        cmzdst
066500010201     C                   eval      cmzfld = vlrfou
066600010201     C                   eval      cmzmbd = vlrhdl
066700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066800021009     C***                if        prmfir = *blanks
066900121107     C                   eval      cmzfla = 'FIVAB00F'
067000121107     C                   eval      cmzmba = 'FIVAB00F'
067100021009     C***                else
067200021009     C***                eval      cmzfla = prmfir
067300021009     C***                eval      cmzmba = prmfir
067400021009     C***                endif
067500010201     C                   eval      cmznrr = *zeros
067600010201     C                   move      §ctrokvb      cmznrr
067700021018     C                   eval      cmzlba = vlrfl1
067800010201     C                   call(e)   'TIS711C'
067900010201     C                   parm                    dscmz
068000010201     C                   parm      *blanks       esito
068100010201     C                   if        %error
068200010201     C                             or cmzerr = '1'
068300010201     C                             or esito  = '1'
068400010201     C                   eval      wrkesito = '3'
068500121107     C                   endif
068600121107     C*
068700121107     C* 3° invio EDIVAB
068800121107     C                   reset                   dscmz
068900121107     C                   move      vlrpoi        cmzdst
069000121107     C                   eval      cmzfld = 'EDIVABWR'
069100121107     C                   eval      cmzmbd = vlrhdl
069200121107     C                   eval      %subst(cmzmbd:1:1) = 'M'
069300121107     C***                if        prmfir = *blanks
069400121107     C                   eval      cmzfla = 'EDIVAB0F'
069500121107     C                   eval      cmzmba = 'EDIVAB0F'
069600121107     C***                else
069700121107     C***                eval      cmzfla = prmfir
069800121107     C***                eval      cmzmba = prmfir
069900121107     C***                endif
070000121107     C                   eval      cmznrr = *zeros
070100121107     C                   move      §ctrokvb      cmznrr
070200121107     C                   eval      cmzlba = vlrfl1
070300121107     C                   call(e)   'TIS711C'
070400121107     C                   parm                    dscmz
070500121107     C                   parm      *blanks       esito
070600121107     C                   if        %error
070700121107     C                             or cmzerr = '1'
070800121107     C                             or esito  = '1'
070900121107     C                   eval      wrkesito = '3'
071000010201     C                   endif
071100990920     C*
071200000613     C                   ENDSR
071300000613     C***
071400120604
071500120604
071600990910
071700000613     C     *inzsr        BEGSR
071800990910     C*
071900990910     C     *entry        plist
072000990920     C                   parm                    tivlrds
072100990921     C                   parm      wrkesito      esito
072200000724     C                   parm                    prmlit
072300000710     C                   parm                    prmfir
072400000613     C*
072500000830     C* CALCOLA LA DATA CORRENTE
072600120604     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072700120604     C                   eval      datcor = %dec(%date() : *ISO)
072800041210     C*
072900041210     C* Chiave su TABEL00F - parziale
073000041210     C     KEYtabP       KLIST
073100041210     C                   KFLD                    tblKUT
073200041210     C                   KFLD                    tblCOD
073300000830     C*
073400000613     C                   ENDSR
073500000613     C***
