000100121221      /TITLE Upload via Internet: traduzione in EDIVABWR/FIVABWWR/FIVATWWR/FIVADWWR
000200121221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600121107     FEDIVABwr  O    E             DISK    usropn
000700121221     FEDIVADwr  O    E             DISK    usropn
000800121107     FFIVABwwr  O    E             DISK    usropn
000900121107     FFIVATwwr  O    E             DISK    usropn
001000120604
001100990908
001200041210     D*------------
001300041210     D* SCHIERE
001400041210     D*------------
001500050131     D skNAZISO        S              3    DIM(1000)
001600050131     D skNAZBAR        S              3    DIM(1000)
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990920     D tivlrds       e ds                  extname(tivlr00f)
002200050131     D tisi95ds      e ds
002300050131     D ds15          e ds
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915     D wrkdata         s               d
002900041210     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080311     D FlgNewBol       s              1    INZ(*blanks)
003400050131     D jNAZ            s              5  0 INZ(*zeros)
003500000830
003600041025     D*------------------
003700041025     D* DS REPERIMENTO NUMERATORE
003800041025     D*------------------
003900050131     D trul33ds      e ds                  inz
004000041025     D*------------------
004100041025     D* DS ARCHITETTURA
004200041025     D*------------------
004300041025     D kpjba         e ds                  inz
004400041025     D*------------------
004500101217
004600990908
004700120611     D*------------------
004800120611     D* DS RIDEFINIZIONE SEGNACOLLO COMPLETO
004900101217     D*------------------
005000120611     D EXPEDITION_REG  DS                  INZ
005100120611     D  EXP_REG_LNP                   3  0
005200120611     D  EXP_REG_LNA                   3  0
005300120611     D  EXP_REG_NRS                   2  0
005400120611     D  EXP_REG_NCD                   7  0
005500120611     D  EXP_REG_ZNC                   2  0
005600120726     D**EXP_REG_CHKD                  1  0
005700120726     D  EXP_REG_TFA                   3  0
005800010201
005900010201
006000990915     C                   time                    wrkdata
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000613     C*
006400050131     C                   EXSR      CARTAB                                       CARICA TABELLE
006500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006600000613     C*
006700010202     C* Effettuo la chiamata al CLLE preposto
006800121107     C                   call(e)   'TITVVTC2'
006900010202     C                   parm                    parccm
007000010202     C                   parm                    parmbr
007100010202     C                   parm      '2'           paropz
007200050131     C*
007300050131     C* Effettuo lancio TISI95 solo x chiusura
007400050131     C                   CLEAR                   TISI95DS
007500050131     C                   EVAL      I95TLA = 'C'
007600050131     C                   CALL      'TISI95R'
007700050131     C                   PARM                    TISI95DS
007800000616     C*
007900010201     C                   seton                                        LR
008000000613
008100000613
008200041210
008300041210     C*--------------------------------------------------------
008400050131     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
008500041210     C*--------------------------------------------------------
008600041210     C     CARTAB        BEGSR
008700041210     C*
008800041210     C* TABELLA '15' - NAZIONI
008900041210     C                   clear                   skNAZISO
009000041210     C                   clear                   skNAZBAR
009100041210     C                   eval      tblKUT = 1
009200041210     C                   eval      tblCOD = '15'
009300041210     C     KEYtabP       setll     tabel00f
009400041210     C     KEYtabP       reade     tabel00f
009500041210     C                   dow       not %eof(tabel00f)
009600041210     C                   if        tblFLG = *blanks
009700041210     C                   movel(p)  tblUNI        ds15
009800041210     C                   add       1             jNAZ
009900041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
010000041210     C                   eval      skNAZISO(jNAZ) = §15COD
010100041210     C                   endif
010200041210     C     KEYtabP       reade     tabel00f
010300041210     C                   enddo
010400041210     C*
010500041210     C                   ENDSR
010600041210     C***
010700041210
010800041210
010900041210
011000910830     C*--------------------------------------------------------
011100121221     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVADWR e FIVABWWR e FIVATWWR
011200910830     C*--------------------------------------------------------
011300040526     C     RWFILE        BEGSR
011400990910     C*
011500990914     C                   if        not %open(tivin00r)
011600990908     C                   open      tivin00r
011700990914     C                   endif
011800121107     C                   if        not %open(fivabwwr)
011900121107     C                   open      fivabwwr
012000990914     C                   endif
012100121107     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012200121221     C                   exsr      prevtvbvd
012300010201     C*
012400010202     C                   if        chkcall = '0'
012500010202     C*
012600121107     C                   if        not %open(edivabwr)
012700121107     C                   open      edivabwr
012800121107     C                   endif
012900121221     C                   if        not %open(edivadwr)
013000121221     C                   open      edivadwr
013100121221     C                   endif
013200121107     C                   if        not %open(fivatwwr)
013300121107     C                   open      fivatwwr
013400010201     C                   endif
013500990910     C*
013600010201     C                   clear                   §CTROKVB          5 0
013700120604     C                   clear                   §CTROKVT          5 0
013800121221     C                   clear                   §CTROKVD          5 0
013900000801     C                   clear                   §CTRMO            5 0
014000000801     C                   clear                   §CTRNO            5 0
014100040910     C*
014200921023     C                   DO        *HIVAL
014300990913     C*
014400990915     C                   READ      tivin00r                               70
014500040910     C                   if        vindta > *blanks
014600000801     C*
014700000801     C                   if        *in70 = *off
014800000801     C                             and
014900000801     C                             (vinflg = *blanks
015000000801     C                              or vinflg = '0'
015100000801     C                              or vinflg = '2')
015200000801     C*
015300000801     C                   clear                   vinmsg
015400000801     C                   eval      vinflg = '1'
015500040910     C*
015600040910     C* Eseguo routine d traduzione
015700121221     C                   exsr      impvbvtvd
015800040802     C*
015900010305     C                   endif
016000000905     C*
016100000905     C                   else
016200000905     C                   eval      vinflg = '1'
016300000905     C                   endif
016400000905     C*
016500000905     C  N70              update    tivin000
016600000905     C*
016700991022     C  N70              ENDdo
016800041210     C*
016900050131     C* Scarico i buffer eventualmente rimasti in sospeso
017000041210     C                   IF        FlgNewBol = '1'
017100121109     C*
017200121109     C* Verifico se richiesto filtro sul codice cliente mittente *IN50 = PRIVALIA BCN
017300121109     C                   SELECT
017400121109     C*
017500121109     C                   WHEN      wCCM = *blanks                               * TUTTO
017600121107     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
017700121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
017800121120     C                                      wORDER_DATA
017900121109     C   50              EVAL      VABCNT = 1
018000121109     C   50              WRITE     EDIVAB00
018100121109     C  N50              WRITE     FIVAB000
018200121109     C*
018300121109     C                   WHEN      wCCM = 'B'                                   * SOLO PRIVALIA BCN
018400121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
018500121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
018600121120     C                                      wORDER_DATA
018700121109     C   50              EVAL      VABCNT = 1
018800121109     C   50              WRITE     EDIVAB00
018900121109     C*
019000121109     C                   WHEN      wCCM = 'L'
019100121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB                * SOLO LOGHISTES SAP
019200121109     C  N50              WRITE     FIVAB000
019300121109     C*
019400121109     C                   ENDSL
019500121109     C                   ENDIF
019600121109     C*
019700010202     C                   endif
019800990910
019900990910     C* Se non ci sono record con errori ...
020000000710     C                   if        §ctrno = 0
020100990910     C* ... restituisco esito OK.
020200990921     C                   eval      wrkesito = '0'
020300990910     C                   else
020400010201     C                   if        §ctrokvb > 0
020500990921     C                   eval      wrkesito = '1'
020600000710     C                   else
020700000710     C                   eval      wrkesito = '2'
020800990910     C                   endif
020900000710     C                   endif
021000990910     C*
021100990914     C                   if        %open(tivin00r)
021200990908     C                   close     tivin00r
021300990914     C                   endif
021400121107     C                   if        %open(fivabwwr)
021500121107     C                   close     fivabwwr
021600990914     C                   endif
021700121107     C                   if        %open(fivatwwr)
021800121107     C                   close     fivatwwr
021900010201     C                   endif
022000121221     C                   if        %open(edivadwr)
022100121221     C                   close     edivadwr
022200121221     C                   endif
022300121107     C                   if        %open(edivabwr)
022400121107     C                   close     edivabwr
022500121107     C                   endif
022600990910     C*
022700010201     C                   if        §ctrokvb > 0
022800000724     C                             and vlrpoi <> *zeros
022900010202     C                   exsr      invio
023000990920     C                   endif
023100990920     C*
023200910830     C                   ENDSR
023300000613     C***
023400990920
023500000801     C*----------------------------------------------------*
023600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023700000801     C*----------------------------------------------------*
023800010201     C     INZVAR        BEGSR
023900000801     C*
024000040802     C                   Z-ADD     *zeros        Num5_0            5 0
024100040802     C                   MOVEL     '0'           FlgCAS            1
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
024600000801     C*----------------------------------------------------*
024700000801     C     DEFCAM        BEGSR
024800000801     C*
024900121107     C                   CLEAR                   EDIVAB00
025000121107     C                   CLEAR                   FIVAB000
025100121107     C                   CLEAR                   FIVAT000
025200020619     C* Imposto i valori di default...
025300120705     C                   Z-ADD     0896769       VABCCM
025400120705     C                   Z-ADD     0896769       VATCCM
025500121221     C                   Z-ADD     0896769       VADCCM
025600050217     C                   Z-ADD     089           VABLNP
025700050217     C                   Z-ADD     089           VATLNP
025800121221     C                   Z-ADD     089           VADLNP
025900120604     C                   Z-ADD     000           VABCTR
026000040823     C                   MOVEL     '1'           VABCBO
026100120604     C                   MOVEL     '2D'          VABCTM
026200020619     C* ... e poi verifico se sono stati passati come parametri
026300020619     C                   IF        vlrppt > *blanks
026400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026600020619     C                   EXSR      CHKNUM
026700020619     C                   IF        PiInt=*on
026800020619     C                   Z-ADD     PiVal         VABCCM
026900020619     C                   Z-ADD     PiVal         VATCCM
027000121221     C                   Z-ADD     PiVal         VADCCM
027100020619     C                   ENDIF
027200040506     C                   ENDIF
027300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027500020619     C                   EXSR      CHKNUM
027600020619     C                   IF        PiInt=*on
027700020619     C                   Z-ADD     PiVal         VABLNP
027800020619     C                   Z-ADD     PiVal         VATLNP
027900121221     C                   Z-ADD     PiVal         VADLNP
028000040506     C                   ENDIF
028100020619     C                   ENDIF
028200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
028300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028400020619     C                   EXSR      CHKNUM
028500020619     C                   IF        PiInt=*on
028600020619     C                   Z-ADD     PiVal         VABCTR
028700040506     C                   ENDIF
028800020619     C                   ENDIF
028900041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
029000041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
029100041210     C                   ENDIF
029200121109     C                   MOVEL     *blanks       wCCM              1
029300121109     C                   IF        %subst(vlrppt:16:1) <> *blanks
029400121109     C                   EVAL      wCCM = %subst(vlrppt:16:1)
029500121109     C                   ENDIF
029600020619     C                   ENDIF
029700000801     C*
029800000801     C                   ENDSR
029900000801     C*----------------------------------------------------*
030000121221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
030100000801     C*----------------------------------------------------*
030200121221     C     IMPVBVTVD     BEGSR
030300041210     C*
030400041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
030500041210     C                   Z-ADD     *zeros        errore            1 0
030600050317     C********
030700120604     C* Tipo record 'F' (dati relativi alla transazione: sender ID, etc...)
030800050317     C********
030900120604     C                   IF        %trim(%subst(vindta:1:1)) = 'F'
031000050317     C                   ENDIF
031100040910     C*
031200041210     C********
031300120604     C* Tipo record 'H' (dati testata: riferimenti, destinatario, contrassegno, peso, etc...)
031400041210     C********
031500120604     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
031600121107     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
031700041210     C                   IF        FlgNewBol = '1'
031800121109     C*
031900121109     C* Verifico se richiesto filtro sul codice cliente mittente *IN50 = PRIVALIA BCN
032000121109     C                   SELECT
032100121109     C*
032200121109     C                   WHEN      wCCM = *blanks                               * TUTTO
032300121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
032400121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
032500121120     C                                      wORDER_DATA
032600121109     C   50              EVAL      VABCNT = 1
032700121109     C   50              WRITE     EDIVAB00
032800121109     C  N50              WRITE     FIVAB000
032900121109     C*
033000121109     C                   WHEN      wCCM = 'B'                                   * SOLO PRIVALIA BCN
033100121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB
033200121120     C   50              EVAL      VABCMR = '0896769 PRIVALIA_BCN'+' '+
033300121120     C                                      wORDER_DATA
033400121109     C   50              EVAL      VABCNT = 1
033500121109     C   50              WRITE     EDIVAB00
033600121109     C*
033700121109     C                   WHEN      wCCM = 'L'
033800121109     C* In base al tipo di traffico scivo o il FIVAB o il EDIVAB                * SOLO LOGHISTES SAP
033900121109     C  N50              WRITE     FIVAB000
034000121109     C*
034100121109     C                   ENDSL
034200121109     C*
034300050131     C                   EVAL      FlgNewBol = '0'
034400041210     C                   ENDIF
034500041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
034600040910     C                   EXSR      INZVAR
034700040910     C                   EXSR      DEFCAM
034800041210     C                   EVAL      FlgNewBol = '1'
034900040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
035000040928     C                   MOVEL     datcor        VABAAS
035100040928     C                   MOVEL     datcor        VATAAS
035200121221     C                   MOVEL     datcor        VADAAS
035300040928     C                   MOVE      datcor        VABMGS
035400040928     C                   MOVE(P)   vlrpoi        VABFGS
035500040928     C                   MOVE(P)   vlrpoi        VATFGS
035600121221     C                   MOVE(P)   vlrpoi        VADFGS
035700120704     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
035800120705     C***                CLEAR                   TRUL33DS
035900120705     C***                EVAL      I33OPE = *zeros
036000120705     C***                EVAL      I33CNU = 302
036100120705     C***                EVAL      I33NUM = 1
036200120705     C***                MOVEL     TRUL33DS      KPJBU
036300120705     C***                CALL      'TRUL33R'
036400120705     C***                PARM                    KPJBA
036500120705     C***                MOVEL     KPJBU         TRUL33DS
036600120705     C***                IF        O33ERR = *zeros
036700120705     C***                Z-ADD     O33NRF        VABNSP
036800120705     C***                Z-ADD     O33NRF        VATNSP
036900121221     C***                Z-ADD     O33NRF        VADNSP
037000120705     C***                ELSE
037100120705     C***                ADD       1             errore
037200120705     C***                EVAL      vinmsg = %trimr(vinmsg)
037300121221     C***                          + ' ' + 'VABNSP VATNSP VADNSP'
037400120705     C***                ENDIF
037500121221     C* ......VABRMN / VABNSP / VATNSP / VADNSP
037600120604     C                   EVAL      PiStr=%trim(%subst(vindta:61:20))
037700041210     C                   EXSR      CHKNUM
037800041210     C                   IF        PiInt=*on
037900041210     C                   Z-ADD     PiVal         VABRMN
038000120705     C                   Z-ADD     PiVal         VABNSP
038100120705     C                   Z-ADD     PiVal         VATNSP
038200121221     C                   Z-ADD     PiVal         VADNSP
038300041210     C                   ELSE
038400041210     C                   Z-ADD     1             VABRMN
038500041210     C                   ADD       1             errore
038600041210     C                   EVAL      vinmsg = %trimr(vinmsg)
038700121221     C                             + ' ' + 'VABRMN VABNSP VATNSP VADNSP'
038800041210     C                   ENDIF
038900041210     C* ......VABRMA
039000121008     C***                EVALR     VABRMA=%trim(%subst(vindta:61:20))
039100121008     C                   EVAL      VABRMA=%trim(%subst(vindta:235:35))
039200121109     C                   SETOFF                                       50
039300121109     C                   IF        %trim(%subst(vindta:235:35)) = 'PRIVALIA_BCN'
039400121109     C                   SETON                                        50
039500121109     C                   ENDIF
039600121120     C* ......VABCMR
039700121120     C                   MOVEL     *blanks       wORDER_DATA       8
039800121120     C                   EVAL      wORDER_DATA = %trim(%subst(vindta:32:8))
039900121008     C* ......VABCTM
040000130114     C***   50              EVAL      VABCTM='7 '
040100050131     C* ......VABRSD
040200120604     C                   EVAL      VABRSD=%trim(%subst(vindta:445:35))
040300050131     C* ......VABIND
040400120604     C                   EVAL      VABIND=%trim(%subst(vindta:480:50))
040500050131     C* ......VABLOD
040600120604     C                   EVAL      VABLOD=%trim(%subst(vindta:530:35))
040700120611     C* ......VABPRD
040800120611     C                   EVAL      VABPRD=%trim(%subst(vindta:585:20))
040900120604     C* ......VABNZD (conversone da formato *ISO a formato "BRT"
041000120604     C                   EVAL      VABNZD=%trim(%subst(vindta:605:20))
041100050131     C                   Z-ADD     1             jNAZ
041200050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
041300050131     C                   IF        %found
041400050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
041500050131     C                   ENDIF
041600050131     C* ......VABCAD
041700120604     C                   EVAL      PiStr=%trim(%subst(vindta:565:20))
041800050131     C                   EXSR      CHKNUM
041900050131     C                   IF        PiInt=*on
042000050131     C                   Z-ADD     PiVal         Num5_0
042100050131     C                   MOVEL(P)  Num5_0        VABCAD
042200050131     C                   ELSE
042300050131     C                   ADD       1             errore
042400050131     C                   EVAL      vinmsg = %trimr(vinmsg)
042500050131     C                             + ' ' + 'VABCAD'
042600050131     C                   ENDIF
042700050131     C* ......VABPRD (reperita mediante TISI95)
042800120604     C                   IF        VABCAD <> *blanks AND
042900120604     C                             VABNZD  = *blanks
043000050131     C                   CLEAR                   TISI95DS
043100050131     C                   EVAL      I95TCN = '3'
043200050131     C                   Z-ADD     datcor        I95DAT
043300050131     C                   EVAL      I95CAP = VABCAD
043400050131     C                   EVAL      I95LOC = VABLOD
043500050131     C                   CALL      'TISI95R'
043600050131     C                   PARM                    TISI95DS
043700050131     C                   EVAL      VABPRD = O95PRV
043800050131     C                   ENDIF
043900101217     C*
044000120611     C* Gestione "Disk B"
044100120705     C                   EVAL      EXPEDITION_REG = %trim(%subst(vindta:161:20))
044200120705     C                   EVAL      VABNRS = EXP_REG_NRS
044300120705     C                   EVAL      VABLNA = EXP_REG_LNA
044400120705     C                   EVAL      VABZNC = EXP_REG_ZNC
044500120726 xxx C                   EVAL      VABNAS = %editc(EXP_REG_NCD:'X')+' '+
044600120726 xxx C                                      %editc(EXP_REG_TFA:'X')
044700121221     C                   EVAL      VATNRS = EXP_REG_NRS
044800121221     C                   EVAL      VADNCD = EXP_REG_NCD
044900121221     C                   EVAL      VADNRS = VABNRS
045000130116     C                   EVAL      VADNCL = 1
045100130114     C   50              EVAL      VADCMR = '0896769 PRIVALIA_BCN'+' '+
045200130114     C                                      wORDER_DATA
045300121221     C                   EVAL      VADCNT = 1
045400121221     C   50              WRITE     EDIVAD00
045500121221     C   50              ADD       1             §CTROKVD
045600120604     C*
045700120604     C* ......VABNCL
045800120604     C                   EVAL      PiStr=%trim(%subst(vindta:181:5))
045900120604     C                   EXSR      CHKNUM
046000120604     C                   IF        PiInt=*on
046100120604     C                   Z-ADD     PiVal         VABNCL
046200120604     C                   ELSE
046300120604     C                   ADD       1             errore
046400120604     C                   EVAL      vinmsg = %trimr(vinmsg)
046500120604     C                             + ' ' + 'VABNCL'
046600120604     C                   ENDIF
046700120604     C* ......VABPKB
046800120604     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
046900120604     C                   EXSR      CHKNUM
047000120604     C                   IF        PiNum=*on
047100120604     C                   Z-ADD     PiVal         VABPKB
047200120604     C                   ELSE
047300120604     C                   ADD       1             errore
047400120604     C                   EVAL      vinmsg = %trimr(vinmsg)
047500120604     C                             + ' ' + 'VABPKB'
047600120604     C                   ENDIF
047700120604     C* ......VABVLB
047800120604     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
047900120604     C                   EXSR      CHKNUM
048000120604     C                   IF        PiNum=*on
048100120604     C                   EVAL      PiVal=PiVal / 1000000
048200120604     C                   Z-ADD     PiVal         VABVLB
048300120604     C                   ELSE
048400120604     C                   ADD       1             errore
048500120604     C                   EVAL      vinmsg = %trimr(vinmsg)
048600120604     C                             + ' ' + 'VABVLB'
048700120604     C                   ENDIF
048800120604     C* ......VABCAS
048900120604     C                   IF        %subst(vindta:210:10) <> *blanks AND
049000120604     C                             %subst(vindta:210:10) <> *zeros  AND
049100120604     C                             %subst(vindta:210:10) <> '000000.000'
049200120604     C                   EVAL      FlgCAS = '1'
049300120604     C                   EVAL      VABVCA=%trim(%subst(vindta:220:5))
049400120604     C                   EVAL      PiStr=%trim(%subst(vindta:210:10))
049500120604     C                   EXSR      CHKNUM
049600120604     C                   IF        PiNum=*on
049700120604     C                   Z-ADD     PiVal         VABCAS
049800120604     C                   ELSE
049900120604     C                   ADD       1             errore
050000120604     C                   EVAL      vinmsg = %trimr(vinmsg)
050100120604     C                             + ' ' + 'VABCAS'
050200120604     C                   ENDIF
050300120604     C                   ENDIF
050400120604     C* ......VABNOT + VABNT2
050500120604     C                   MOVEL     *blanks       wNOTE            70
050600120605     C                   EVAL      wNOTE=%trim(%subst(vindta:655:300))+
050700120605     C                                   %trim(%subst(vindta:955:300))
050800120604     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
050900120604     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
051000120604     C* ......VATNOT_A (tel referente consegna)
051100120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
051200120627     C                             %trim(%subst(vindta:625:15)):15:0):'4'))
051300050131     C                   EVAL      VATTRC = 'A'
051400120604     C                   IF        VATNOT <> *blanks AND
051500120604     C                             VATNOT <> *zeros
051600121107     C                   WRITE     FIVAT000
051700120604     C                   ADD       1             §CTROKVT
051800050131     C                   ENDIF
051900120604     C* ......VATNOT_B (tel referente consegna)
052000120627     C                   EVAL      VATNOT = %trim(%editc(%dec(
052100120627     C                             %trim(%subst(vindta:640:15)):15:0):'4'))
052200101217     C                   EVAL      VATTRC = 'B'
052300120604     C                   IF        VATNOT <> *blanks AND
052400120604     C                             VATNOT <> *zeros
052500121107     C                   WRITE     FIVAT000
052600120604     C                   ADD       1             §CTROKVT
052700101217     C                   ENDIF
052800050131     C*
052900050131     C                   ENDIF
053000050131     C*
053100050131     C********
053200120604     C* Tipo record 'L' (dati dettaglio: barcode, peso, volume, colli, etc...)
053300050131     C********
053400120604     C                   IF        %trim(%subst(vindta:1:1)) = 'L'
053500041210     C                   ENDIF
053600040910     C*
053700040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
053800040910     C                   IF        FlgCAS <> '0'
053900040929     C                   IF        VABCBO = '1'
054000040910     C                   EVAL      VABCBO = '4'
054100040910     C                   ELSE
054200040929     C                   EVAL      VABCBO = '6'
054300040910     C                   ENDIF
054400040929     C                   ENDIF
054500040910     C*
054600040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054700040910     C                   EXSR      CHKIMPDIV
054800010202     C*
054900000801     C* Ebbene...
055000000801     C                   ADD       1             §CTRMO
055100010201     C                   IF        errore <> *zeros
055200000801     C                   ADD       1             §CTRNO
055300000801     C                   EVAL      vinflg = '2'
055400000801     C                   ELSE
055500010201     C                   ADD       1             §CTROKVB
055600000801     C                   ENDIF
055700000801     C*
055800000801     C                   ENDSR
055900010202     C*----------------------------------------------------*
056000121221     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "AGGIUNTIVI"
056100010202     C*----------------------------------------------------*
056200121221     C     PREVTVBVD     BEGSR
056300010202     C*
056400121221     C* Compongo il nome del membro da dare al FIVATWWR e EDIVADWR
056500010202     C                   eval      parmbr = vlrhdl
056600010202     C                   movel     'M'           parmbr
056700041210     C                   eval      parccm = vlrksc
056800010202     C                   eval      paropz = '1'
056900010202     C* Effettuo la chiamata al CLLE preposto
057000121221     C                   call(e)   'TITVVTDC2'
057100010202     C                   parm                    parccm
057200010202     C                   parm                    parmbr
057300010202     C                   parm                    paropz
057400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057500010202     C                   if        %error
057600010202     C                   movel     '1'           chkcall
057700010202     C                   else
057800010202     C                   movel     '0'           chkcall
057900010202     C                   endif
058000010202     C*
058100010202     C                   ENDSR
058200000801     C*----------------------------------------------------*
058300000801     C*  CONTROLLO NUMERICITA' CAMPI
058400000801     C*----------------------------------------------------*
058500000801     C     CHKNUM        BEGSR
058600000801     C*
058700000801     C                   call(e)   'ISNUMERIC'
058800000801     C                   PARM                    PiStr            30
058900041210     C                   PARM      '.'           PiDecChr          1
059000000801     C                   PARM      *ZEROS        PiVal            30 9
059100000801     C                   PARM      '0'           PiInt             1
059200000801     C                   PARM      '0'           PiNum             1
059300000801     C                   IF        %error
059400000801     C                   EVAL      PiInt=*off
059500000801     C                   ENDIF
059600000801     C*
059700000801     C                   ENDSR
059800000801     C***
059900000801
060000011113
060100011113     C*----------------------------------------------------*
060200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060300011113     C*----------------------------------------------------*
060400011113     C     CHKIMPDIV     BEGSR
060500011113     C*
060600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060700011113     C                   Z-ADD     *zeros        wrkDec            9 9
060800011113     C*
060900011113     C* Come prima cosa effettuo considerazioni sulla divisa
061000011113     C                   IF        vabIAS > *zeros
061100011113     C                   IF        vabVAS <> 'EUR'
061200011113     C                   EVAL      vabVAS =  'ITL'
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C                   IF        vabCAS > *zeros
061700011113     C                   IF        vabVCA <> 'EUR'
061800011113     C                   EVAL      vabVCA =  'ITL'
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C                   IF        vabVMD > *zeros
062300020305     C                   IF        vabVAD <> 'EUR'
062400011113     C                   EVAL      vabVAD =  'ITL'
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062900011113     C                   Z-ADD     vabIAS        wrkDec
063000011113     C                   IF        wrkDec > *zeros
063100011113     C                   IF        vabVAS = 'ITL'
063200011113     C                   EVAL      vabIAS = *zeros
063300011113     C                   ENDIF
063400011113     C                   ENDIF
063500011113     C*
063600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063700011113     C                   Z-ADD     vabCAS        wrkDec
063800011113     C                   IF        wrkDec > *zeros
063900011113     C                   IF        vabVCA = 'ITL'
064000011113     C                   EVAL      vabCAS = *zeros
064100011113     C                   ENDIF
064200011113     C                   ENDIF
064300011113     C*
064400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064500011113     C                   Z-ADD     vabVMD        wrkDec
064600011113     C                   IF        wrkDec > *zeros
064700011113     C                   IF        vabVAD = 'ITL'
064800011113     C                   EVAL      vabVMD = *zeros
064900011113     C                   ENDIF
065000011113     C                   ENDIF
065100011113     C*
065200011113     C                   ENDSR
065300011113     C***
065400011113
065500011113
065600000801
065700000801
065800990920      /TITLE Invio dei dati al punto operativo.
065900010202     C     invio         BEGSR
066000990920     C*
066100121107     C* 1° invio FIVAT
066200010201     C                   reset                   dscmz
066300010201     C                   move      vlrpoi        cmzdst
066400121107     C                   eval      cmzfld = 'FIVATWWR'
066500010201     C                   eval      cmzmbd = vlrhdl
066600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066700021009     C***                if        prmfir = *blanks
066800121107     C                   eval      cmzfla = 'FIVAT00F'
066900121107     C                   eval      cmzmba = 'FIVAT00F'
067000021009     C***                else
067100021009     C***                eval      cmzfla = prmfir
067200021009     C***                eval      cmzmba = prmfir
067300021009     C***                endif
067400010201     C                   eval      cmznrr = *zeros
067500020305     C                   move      §ctrokvt      cmznrr
067600021018     C                   eval      cmzlba = vlrfl1
067700010201     C                   call(e)   'TIS711C'
067800010201     C                   parm                    dscmz
067900010201     C                   parm      *blanks       esito
068000010205     C                   if        %error
068100010205     C                             or cmzerr = '1'
068200010205     C                             or esito  = '1'
068300010205     C                   eval      wrkesito = '3'
068400121107     C                   endif
068500010201     C*
068600121107     C* 2° invio FIVAB
068700010201     C                   reset                   dscmz
068800010201     C                   move      vlrpoi        cmzdst
068900010201     C                   eval      cmzfld = vlrfou
069000010201     C                   eval      cmzmbd = vlrhdl
069100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069200021009     C***                if        prmfir = *blanks
069300121107     C                   eval      cmzfla = 'FIVAB00F'
069400121107     C                   eval      cmzmba = 'FIVAB00F'
069500021009     C***                else
069600021009     C***                eval      cmzfla = prmfir
069700021009     C***                eval      cmzmba = prmfir
069800021009     C***                endif
069900010201     C                   eval      cmznrr = *zeros
070000010201     C                   move      §ctrokvb      cmznrr
070100021018     C                   eval      cmzlba = vlrfl1
070200010201     C                   call(e)   'TIS711C'
070300010201     C                   parm                    dscmz
070400010201     C                   parm      *blanks       esito
070500010201     C                   if        %error
070600010201     C                             or cmzerr = '1'
070700010201     C                             or esito  = '1'
070800010201     C                   eval      wrkesito = '3'
070900121107     C                   endif
071000121107     C*
071100121107     C* 3° invio EDIVAB
071200121107     C                   reset                   dscmz
071300121107     C                   move      vlrpoi        cmzdst
071400121107     C                   eval      cmzfld = 'EDIVABWR'
071500121107     C                   eval      cmzmbd = vlrhdl
071600121107     C                   eval      %subst(cmzmbd:1:1) = 'M'
071700121107     C***                if        prmfir = *blanks
071800121107     C                   eval      cmzfla = 'EDIVAB0F'
071900121107     C                   eval      cmzmba = 'EDIVAB0F'
072000121107     C***                else
072100121107     C***                eval      cmzfla = prmfir
072200121107     C***                eval      cmzmba = prmfir
072300121107     C***                endif
072400121107     C                   eval      cmznrr = *zeros
072500121107     C                   move      §ctrokvb      cmznrr
072600121107     C                   eval      cmzlba = vlrfl1
072700121107     C                   call(e)   'TIS711C'
072800121107     C                   parm                    dscmz
072900121107     C                   parm      *blanks       esito
073000121107     C                   if        %error
073100121107     C                             or cmzerr = '1'
073200121107     C                             or esito  = '1'
073300121107     C                   eval      wrkesito = '3'
073400010201     C                   endif
073500121221     C*
073600121221     C* 4° invio EDIVAD
073700121221     C                   reset                   dscmz
073800121221     C                   move      vlrpoi        cmzdst
073900121221     C                   eval      cmzfld = 'EDIVADWR'
074000121221     C                   eval      cmzmbd = vlrhdl
074100121221     C                   eval      %subst(cmzmbd:1:1) = 'M'
074200121221     C***                if        prmfir = *blanks
074300121221     C                   eval      cmzfla = 'EDIVAD0F'
074400121221     C                   eval      cmzmba = 'EDIVAD0F'
074500121221     C***                else
074600121221     C***                eval      cmzfla = prmfir
074700121221     C***                eval      cmzmba = prmfir
074800121221     C***                endif
074900121221     C                   eval      cmznrr = *zeros
075000121221     C                   move      §ctrokvd      cmznrr
075100121221     C                   eval      cmzlba = vlrfl1
075200121221     C                   call(e)   'TIS711C'
075300121221     C                   parm                    dscmz
075400121221     C                   parm      *blanks       esito
075500121221     C                   if        %error
075600121221     C                             or cmzerr = '1'
075700121221     C                             or esito  = '1'
075800121221     C                   eval      wrkesito = '3'
075900121221     C                   endif
076000990920     C*
076100000613     C                   ENDSR
076200000613     C***
076300120604
076400120604
076500990910
076600000613     C     *inzsr        BEGSR
076700990910     C*
076800990910     C     *entry        plist
076900990920     C                   parm                    tivlrds
077000990921     C                   parm      wrkesito      esito
077100000724     C                   parm                    prmlit
077200000710     C                   parm                    prmfir
077300000613     C*
077400000830     C* CALCOLA LA DATA CORRENTE
077500120604     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077600120604     C                   eval      datcor = %dec(%date() : *ISO)
077700041210     C*
077800041210     C* Chiave su TABEL00F - parziale
077900041210     C     KEYtabP       KLIST
078000041210     C                   KFLD                    tblKUT
078100041210     C                   KFLD                    tblCOD
078200000830     C*
078300000613     C                   ENDSR
078400000613     C***
