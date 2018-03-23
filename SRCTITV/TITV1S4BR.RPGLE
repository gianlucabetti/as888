000100091022      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500091012     FTITV1S41L uF A E           k DISK    rename(TITV1S400:TITV1S401)
000600091012     F                                     prefix(VAB_)
000700091012     FTITV1S42L uF A E           k DISK    rename(TITV1S400:TITV1S402)
000800091012     F                                     prefix(VAT_)
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600060725     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100091012     D rrnum           s              6  0 INZ(*zeros)
002200091022     D edivab_IN     e ds                  extname(edivab0f)
002300091210     D edivab_O_VAT  e ds                  extname(edivab0f) prefix(VAT_)
002400091210     D edivab_O_VAB  e ds                  extname(edivab0f) prefix(VAB_)
002500091210     D wSavPKB         s                   like(VAB_VABPKB)
002600091012     D wTV1S4KEY       s                   like(VAB_TV1S4KEY)
002700091012     D wTV1S4FLGT      s                   like(VAB_TV1S4FLGT)
002800010201
002900101125     D*------------------
003000101125     D* DS REPERIMENTO NUMERATORE
003100101125     D*------------------
003200101125     D trul33ds      e ds                  inz
003300101125     D*------------------
003400101125     D* DS ARCHITETTURA
003500101125     D*------------------
003600101125     D kpjba         e ds                  inz
003700101125
003800091006
003900091006     D*------------------
004000091006     D* LINKING A DEFINIZIONI ESTERNE
004100091006     D*------------------
004200091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004300091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004400091006
004500010201
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900091110     C*
005000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005100050201     C*
005200050201     C* Effettuo lancio TISI95 solo x chiusura
005300050201     C                   CLEAR                   TISI95DS
005400050201     C                   EVAL      I95TLA = 'C'
005500050201     C                   CALL      'TISI95R'
005600050201     C                   PARM                    TISI95DS
005700000616     C*
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200091022     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900091012     C*
007000091012     C                   clear                   §CTROKVB          5 0
007100091012     C                   clear                   §CTROKVT          5 0
007200091012     C                   clear                   §CTRMO            5 0
007300091012     C                   clear                   §CTRNO            5 0
007400990910     C*
007500921023     C                   DO        *HIVAL
007600990913     C*
007700990915     C                   READ      tivin00r                               70
007800050627     C                   if        vindta > *blanks
007900000613     C                   add       1             rrnum
008000000801     C*
008100000801     C                   if        *in70 = *off
008200000801     C                             and
008300000801     C                             (vinflg = *blanks
008400000801     C                              or vinflg = '0'
008500000801     C                              or vinflg = '2')
008600000801     C*
008700000801     C                   clear                   vinmsg
008800000801     C                   eval      vinflg = '1'
008900010305     C*
009000091022     C                   clear                   edivab_in
009100050628     C                   exsr      impvab                                       => carico VAB
009200060725     C                   exsr      wrivab                                       => scarico VAB
009300000905     C*
009400000905     C                   else
009500000905     C                   eval      vinflg = '1'
009600050628     C                   endif
009700000905     C                   endif
009800000905     C*
009900000905     C  N70              update    tivin000
010000000905     C*
010100991022     C  N70              ENDdo
010200990910
010300091012
010400990910     C* Se non ci sono record con errori ...
010500000710     C                   if        §ctrno = 0
010600990910     C* ... restituisco esito OK.
010700990921     C                   eval      wrkesito = '0'
010800990910     C                   else
010900010201     C                   if        §ctrokvb > 0
011000990921     C                   eval      wrkesito = '1'
011100000710     C                   else
011200000710     C                   eval      wrkesito = '2'
011300990910     C                   endif
011400000710     C                   endif
011500990910     C*
011600990914     C                   if        %open(tivin00r)
011700990908     C                   close     tivin00r
011800990914     C                   endif
011900990920     C*
012000910830     C                   ENDSR
012100000613     C***
012200010305
012300010305     C*----------------------------------------------------*
012400091012     C*  SCARICAMENTO BUFFER RECORDS VAB
012500010305     C*----------------------------------------------------*
012600020305     C     WRIVAB        BEGSR
012700091012     C*
012800100312     C                   EVAL      wTV1S4KEY  = '0'+%editc(wCCM1:'X')+'_BOLLE'
012900091021     C                   EVAL      wTV1S4FLGT = %trim(%subst(vindta:507:1))
013000091012     C*
013100091022     C* Verifico se già esistono i record EDIVAT correlati
013200091014     C     KEY1s4_P      SETLL     TITV1S42L
013300091012     C*
013400091012     C                   IF        %equal(TITV1S42L)
013500091014     C     KEY1s4_P      READE     TITV1S42L
013600091012     C                   DOW       not %eof(TITV1S42L)
013700091210     C                   EVAL      wSavPKB       = VAT_VABPKB
013800091210     C                   EVAL      edivab_o_vat  = edivab_in
013900091210     C                   EVAL      VAT_VABPKB    = wSavPKB
014000091012     C                   EVAL      VAT_TV1S4KEY  = wTV1S4KEY
014100091012     C                   EVAL      VAT_TV1S4FLGT = wTV1S4FLGT
014200091012     C                   EVAL      VAT_TV1S4VAB  = 'X'
014300091110     C   50              EVAL      VAT_TV1S4VAT  = 'X'
014400091012     C                   UPDATE    TITV1S402
014500091014     C     KEY1s4_P      READE     TITV1S42L
014600091012     C                   ENDDO
014700091012     C*
014800091012     C                   ELSE
014900091012     C*
015000091014     C     KEY1s4_P      CHAIN     TITV1S41L
015100091012     C*
015200091012     C                   IF        %found(TITV1S41L)
015300091210     C                   EVAL      wSavPKB       = VAB_VABPKB
015400091210     C                   EVAL      edivab_o_vab  = edivab_in
015500091210     C                   EVAL      VAB_VABPKB    = wSavPKB
015600091012     C                   EVAL      VAB_TV1S4KEY  = wTV1S4KEY
015700091012     C                   EVAL      VAB_TV1S4FLGT = wTV1S4FLGT
015800091012     C                   EVAL      VAB_TV1S4VAB  = 'X'
015900091110     C   50              EVAL      VAB_TV1S4VAT  = 'X'
016000091012     C                   UPDATE    TITV1S401
016100091012     C*
016200091012     C                   ELSE
016300091012     C*
016400100118     C                   CLEAR                   VAB_VABPKB
016500091210     C                   EVAL      edivab_o_vab  = edivab_in
016600091210     C                   EVAL      VAB_VABPKB    = wSavPKB
016700091012     C                   EVAL      VAB_TV1S4KEY  = wTV1S4KEY
016800091012     C                   EVAL      VAB_TV1S4FLGT = wTV1S4FLGT
016900091012     C                   EVAL      VAB_TV1S4VAB  = 'X'
017000091110     C   50              EVAL      VAB_TV1S4VAT  = 'X'
017100091012     C                   WRITE     TITV1S401
017200091012     C                   ENDIF
017300091012     C*
017400091012     C                   ENDIF
017500010305     C*
017600010305     C                   ENDSR
017700990920
017800000801     C*----------------------------------------------------*
017900091012     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018000000801     C*----------------------------------------------------*
018100010201     C     INZVAR        BEGSR
018200000801     C*
018300040802     C                   Z-ADD     *zeros        Num5_0            5 0
018400040802     C                   MOVEL     '0'           FlgCAS            1
018500000801     C*
018600000801     C                   ENDSR
018700000801     C*----------------------------------------------------*
018800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018900000801     C*----------------------------------------------------*
019000000801     C     DEFCAM        BEGSR
019100000801     C*
019200020619     C* Imposto i valori di default...
019300091102     C                   Z-ADD     100           VABCTR
019400091021     C                   MOVEL     '1'           VABCBO
019500091021     C                   MOVEL     '7Q'          VABCTM
019600020619     C* ... e poi verifico se sono stati passati come parametri
019700020619     C                   IF        vlrppt > *blanks
019800100312     C                   IF        %subst(vlrppt:1:7) <> *blanks
019900100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020000100312     C                   EXSR      CHKNUM
020100100312     C                   IF        PiInt=*on
020200100312     C                   Z-ADD     PiVal         wCCM1             7 0
020300100312     C                   ENDIF
020400100312     C                   ENDIF
020500100312     C                   IF        %subst(vlrppt:8:7) <> *blanks
020600100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:7))
020700100312     C                   EXSR      CHKNUM
020800100312     C                   IF        PiInt=*on
020900100312     C                   Z-ADD     PiVal         wCCM2             7 0
021000100312     C                   ENDIF
021100100312     C                   ENDIF
021200100312     C                   IF        %subst(vlrppt:15:7) <> *blanks
021300100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:7))
021400100312     C                   EXSR      CHKNUM
021500100312     C                   IF        PiInt=*on
021600100312     C                   Z-ADD     PiVal         wCCM3             7 0
021700100312     C                   ENDIF
021800100312     C                   ENDIF
021900100312     C                   IF        %subst(vlrppt:22:3) <> *blanks
022000100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:22:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABLNP
022400040506     C                   ENDIF
022500020619     C                   ENDIF
022600100312     C                   IF        %subst(vlrppt:25:3) <> *blanks
022700100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:25:3))
022800020619     C                   EXSR      CHKNUM
022900020619     C                   IF        PiInt=*on
023000020619     C                   Z-ADD     PiVal         VABCTR
023100040506     C                   ENDIF
023200020619     C                   ENDIF
023300100312     C                   IF        %subst(vlrppt:28:2) = 'VB'
023400091110     C                   SETON                                        50
023500091110     C                   ELSE
023600091110     C                   SETOFF                                       50
023700091110     C                   ENDIF
023800100312     C                   IF        %subst(vlrppt:30:1) = 'L'
023900100312     C                   SETON                                        51
024000100312     C                   ELSE
024100100312     C                   SETOFF                                       51
024200100312     C                   ENDIF
024300100312     C                   IF        %subst(vlrppt:31:8) <> *blanks
024400100312     C                   MOVEL     *blanks       wCMR              8
024500100312     C                   EVAL      wCMR=%trim(%subst(vlrppt:31:8))
024600100312     C                   ENDIF
024700100312     C                   IF        %subst(vlrppt:39:8) <> *blanks
024800100312     C                   MOVEL     *blanks       wEMAIL            8
024900100312     C                   EVAL      wEMAIL=%trim(%subst(vlrppt:39:8))
025000100312     C                   ENDIF
025100091110     C                   ENDIF
025200000801     C*
025300000801     C                   ENDSR
025400000801     C*----------------------------------------------------*
025500091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
025600000801     C*----------------------------------------------------*
025700040823     C     IMPVAB        BEGSR
025800040823     C*
025900020305     C                   EXSR      INZVAR
026000020305     C                   EXSR      DEFCAM
026100010305     C*
026200000801     C                   Z-ADD     *zeros        errore            1 0
026300040526     C                   MOVE      datcor        VABMGS
026400060529     C                   MOVE(P)   vlrpoi        VABFGS
026500050628     C*
026600091012     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
026700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026900020117     C* ==
027000091012     C                   EVAL      VABCBO=%trim(%subst(vindta:35:2))
027100091012     C                   EVAL      VABRD2=%trim(%subst(vindta:76:35))
027200091012     C                   EVAL      VABIND=%trim(%subst(vindta:111:35))
027300091012     C                   EVAL      VABLOD=%trim(%subst(vindta:155:35))
027400091012     C                   EVAL      VABPRD=%trim(%subst(vindta:190:2))
027500091012     C                   EVAL      VABNZD=%trim(%subst(vindta:192:3))
027600091012     C                   EVAL      VABNAS=%trim(%subst(vindta:222:15))
027700091012     C                   EVAL      VABRMA=%trim(%subst(vindta:312:15))
027800091022     C***                EVAL      VABNOT=%trim(%subst(vindta:344:35))
027900091022     C***                EVAL      VABNT2=%trim(%subst(vindta:379:35))
028000091021     C                   EVAL      VABTC1=%trim(%subst(vindta:507:1))
028100091012     C                   EVAL      VABCAD=%trim(%subst(vindta:146:9))
028200091012     C* LNP
028300091012     C                   EVAL      PiStr=%trim(%subst(vindta:10:4))
028400091012     C                   EXSR      CHKNUM
028500091012     C                   IF        PiInt=*on
028600091012     C                   Z-ADD     PiVal         VABLNP
028700091012     C                   ELSE
028800091012     C                   ADD       1             errore
028900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
029000091012     C                             + ' ' + 'VABLNP'
029100091012     C                   ENDIF
029200091012     C* AAS
029300091012     C                   EVAL      PiStr=%trim(%subst(vindta:14:5))
029400091012     C                   EXSR      CHKNUM
029500091012     C                   IF        PiInt=*on
029600091012     C                   Z-ADD     PiVal         VABAAS
029700091012     C                   ELSE
029800091012     C                   ADD       1             errore
029900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
030000091012     C                             + ' ' + 'VABAAS'
030100091012     C                   ENDIF
030200091012     C* NRS
030300091012     C                   EVAL      PiStr=%trim(%subst(vindta:24:3))
030400091012     C                   EXSR      CHKNUM
030500091012     C                   IF        PiInt=*on
030600091012     C                   Z-ADD     PiVal         VABNRS
030700091012     C                   ELSE
030800091012     C                   ADD       1             errore
030900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
031000091012     C                             + ' ' + 'VABNRS'
031100091012     C                   ENDIF
031200091012     C* NSP
031300091012     C                   EVAL      PiStr=%trim(%subst(vindta:27:8))
031400091012     C                   EXSR      CHKNUM
031500091012     C                   IF        PiInt=*on
031600091012     C                   Z-ADD     PiVal         VABNSP
031700091012     C                   ELSE
031800091012     C                   ADD       1             errore
031900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
032000091012     C                             + ' ' + 'VABNSP'
032100091012     C                   ENDIF
032200060529     C* RMN
032300101125     C***                EVAL      PiStr=%trim(%subst(vindta:296:16))
032400101125     C***                EXSR      CHKNUM
032500101125     C***                IF        PiInt=*on
032600101125     C***                Z-ADD     PiVal         VABRMN
032700101125     C***                ELSE
032800101125     C***                ADD       1             errore
032900101125     C***                EVAL      vinmsg = %trimr(vinmsg)
033000101125     C***                          + ' ' + 'VABRMN'
033100101125     C***                ENDIF
033200101125     C*
033300101125     C* RMN => Stacco un numeratore da AZNUM
033400101125     C                   clear                   TRUL33DS
033500101125     C                   eval      I33OPE = *zeros
033600101125     C                   eval      I33CNU = 302
033700101125     C                   eval      I33NUM = 1
033800101125     C                   movel     TRUL33DS      KPJBU
033900101125     C                   call      'TRUL33R'
034000101125     C                   parm                    KPJBA
034100101125     C                   movel     KPJBU         TRUL33DS
034200101125     C*
034300110114     C                   movel     datcor        O33NRF
034400110114     C                   eval      VABRMN = O33NRF
034500091006     C*
034600040506     C* Reperisco la provincia dal CAP e dalla località
034700040526     C                   IF        VABCAD <> *blanks AND
034800040526     C                             VABPRD  = *blanks
034900040506     C                   CLEAR                   TISI95DS
035000040506     C                   EVAL      I95TCN = '3'
035100040506     C                   Z-ADD     datcor        I95DAT
035200040506     C                   EVAL      I95CAP = VABCAD
035300040506     C                   EVAL      I95LOC = VABLOD
035400050627     C                   EVAL      I95NAR = VABNZD
035500040506     C                   CALL      'TISI95R'
035600040506     C                   PARM                    TISI95DS
035700040506     C                   EVAL      VABPRD = O95PRV
035800040506     C                   ENDIF
035900091006     C* NCL
036000091015     C                   EVAL      VABNCL = 1
036100091015     C***                EVAL      PiStr=%trim(%subst(vindta:237:6))
036200091015     C***                EXSR      CHKNUM
036300091015     C***                IF        PiInt=*on
036400091015     C***                ADD       PiVal         VABNCL
036500091015     C***                ELSE
036600091015     C***                ADD       1             errore
036700091015     C***                EVAL      vinmsg = %trimr(vinmsg)
036800091015     C***                          + ' ' + 'VABNCL'
036900091015     C***                ENDIF
037000040506     C* PKB
037100091210     C***                EVAL      PiStr=%trim(%subst(vindta:243:9))
037200091210     C***                EXSR      CHKNUM
037300091210     C***                IF        PiNum=*on
037400091210     C***                Z-ADD(H)  PiVal         VABPKB
037500091210     C***                ELSE
037600091210     C***                ADD       1             errore
037700091210     C***                EVAL      vinmsg = %trimr(vinmsg)
037800091210     C***                          + ' ' + 'VABPKB'
037900091210     C***                ENDIF
038000091006     C* VLB
038100091015     C***                EVAL      PiStr=%trim(%subst(vindta:252:7))
038200091015     C***                EXSR      CHKNUM
038300091015     C***                IF        PiNum=*on
038400091015     C***                Z-ADD(H)  PiVal         VABVLB
038500091015     C***                ELSE
038600091015     C***                ADD       1             errore
038700091015     C***                EVAL      vinmsg = %trimr(vinmsg)
038800091015     C***                          + ' ' + 'VABVLB'
038900091015     C***                ENDIF
039000060529     C*
039100091006     C* Gestione estensioni "VAT"
039200091006     C                   exsr      exeVATA
039300091007     C                   exsr      exeVATB
039400010205     C*
039500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039600040802     C                   IF        FlgCAS <> '0'
039700071207     C                   IF        VABCBO = '1'
039800071207     C                   EVAL      VABCBO = '4'
039900071207     C                   ENDIF
040000071207     C                   IF        VABCBO = '2'
040100071207     C                   EVAL      VABCBO = '6'
040200071207     C                   ENDIF
040300071207     C                   ENDIF
040400020305     C*
040500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040600011113     C                   EXSR      CHKIMPDIV
040700010202     C*
040800000801     C* Ebbene...
040900091012     C                   ADD       1             §CTRMO
041000010201     C                   IF        errore <> *zeros
041100091012     C                   ADD       1             §CTRNO
041200000801     C                   EVAL      vinflg = '2'
041300091012     C                   ELSE
041400010201     C                   ADD       1             §CTROKVB
041500091012     C                   ENDIF
041600000801     C*
041700000801     C                   ENDSR
041800050628     C*----------------------------------------------------*
041900091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
042000050628     C*----------------------------------------------------*
042100071207     C     EXEVATE       BEGSR
042200071207     C*
042300091009     C***                EVAL      VATTRC = 'E'
042400091009     C***                EVAL      VATNOT = %subst(vindta:340:50)
042500091012     C***                IF        VATNOT <> *blanks
042600091022     C***                write     EDIVAT00
042700091012     C***                add       1             §CTROKVT
042800091012     C***                ENDIF
042900050628     C*
043000050628     C                   ENDSR
043100091006     C*----------------------------------------------------*
043200091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
043300091006     C*----------------------------------------------------*
043400091006     C     EXEVATA       BEGSR
043500091006     C*
043600091012     C***                EVAL      VATTRC = 'A'
043700091012     C***                EVAL      VATNOT = %subst(vindta:340:50)
043800091012     C***                IF        VATNOT <> *blanks
043900091022     C***                write     EDIVAT00
044000091012     C***                add       1             §CTROKVT
044100091012     C***                ENDIF
044200091006     C*
044300091006     C                   ENDSR
044400091007     C*----------------------------------------------------*
044500091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
044600091007     C*----------------------------------------------------*
044700091007     C     EXEVATB       BEGSR
044800091007     C*
044900091012     C***                EVAL      VATTRC = 'B'
045000091012     C***                EVAL      VATNOT = %subst(vindta:330:10)
045100091012     C***                IF        VATNOT <> *blanks
045200091022     C***                write     EDIVAT00
045300091012     C***                add       1             §CTROKVT
045400091012     C***                ENDIF
045500091007     C*
045600091007     C                   ENDSR
045700000801     C*----------------------------------------------------*
045800000801     C*  CONTROLLO NUMERICITA' CAMPI
045900000801     C*----------------------------------------------------*
046000091006     C     CHKNUM        BEGSR
046100091006     C*
046200091006     C                   IF        PiDecChr = *blanks
046300091006     C                   EVAL      PiDecChr = ','
046400091006     C                   ENDIF
046500091006     C*
046600091006     C                   callp(e)  UBISNUM_Check(PiStr
046700091006     C                                          :PiDecChr
046800091006     C                                          :PiVal
046900091006     C                                          :PiNum
047000091006     C                                          :PiInt)
047100091006     C*
047200091006     C                   IF        %error
047300091006     C                   EVAL      PiInt=*off
047400091006     C                   ENDIF
047500000801     C*
047600000801     C                   ENDSR
047700000801     C***
047800000801
047900011113
048000011113     C*----------------------------------------------------*
048100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048200011113     C*----------------------------------------------------*
048300011113     C     CHKIMPDIV     BEGSR
048400011113     C*
048500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048600011113     C                   Z-ADD     *zeros        wrkDec            9 9
048700011113     C*
048800011113     C* Come prima cosa effettuo considerazioni sulla divisa
048900011113     C                   IF        vabIAS > *zeros
049000011113     C                   IF        vabVAS <> 'EUR'
049100011113     C                   EVAL      vabVAS =  'ITL'
049200011113     C                   ENDIF
049300011113     C                   ENDIF
049400011113     C*
049500011113     C                   IF        vabCAS > *zeros
049600011113     C                   IF        vabVCA <> 'EUR'
049700011113     C                   EVAL      vabVCA =  'ITL'
049800011113     C                   ENDIF
049900011113     C                   ENDIF
050000011113     C*
050100011113     C                   IF        vabVMD > *zeros
050200020305     C                   IF        vabVAD <> 'EUR'
050300011113     C                   EVAL      vabVAD =  'ITL'
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050800011113     C                   Z-ADD     vabIAS        wrkDec
050900011113     C                   IF        wrkDec > *zeros
051000011113     C                   IF        vabVAS = 'ITL'
051100011113     C                   EVAL      vabIAS = *zeros
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051600011113     C                   Z-ADD     vabCAS        wrkDec
051700011113     C                   IF        wrkDec > *zeros
051800011113     C                   IF        vabVCA = 'ITL'
051900011113     C                   EVAL      vabCAS = *zeros
052000011113     C                   ENDIF
052100011113     C                   ENDIF
052200011113     C*
052300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052400011113     C                   Z-ADD     vabVMD        wrkDec
052500011113     C                   IF        wrkDec > *zeros
052600011113     C                   IF        vabVAD = 'ITL'
052700011113     C                   EVAL      vabVMD = *zeros
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C                   ENDSR
053200011113     C***
053300070411
053400091012
053500091012
053600070411     C     *pssr         BEGSR
053700070411     C*
053800070411     C                   if        %open(tivin00r)
053900070411     C                   close     tivin00r
054000070411     C                   endif
054100070411     C*
054200070411     C                   eval      wrkesito = '2'
054300070411     C*
054400070411     C                   seton                                        LR
054500070411     C*
054600070411     C                   ENDSR     '*CANCL'
054700070411     C***
054800070411
054900990910
055000000613     C     *inzsr        BEGSR
055100990910     C*
055200990910     C     *entry        plist
055300990920     C                   parm                    tivlrds
055400990921     C                   parm      wrkesito      esito
055500000724     C                   parm                    prmlit
055600000710     C                   parm                    prmfir
055700091012     C*
055800091012     C* Definizione chiavi
055900091014     C* TITV1S4*L - Parziale
056000091014     C     KEY1s4_P      klist
056100091012     C                   kfld                    wTV1S4KEY
056200091012     C                   kfld                    VABAAS
056300091012     C                   kfld                    VABLNP
056400091012     C                   kfld                    VABNRS
056500091012     C                   kfld                    VABNSP
056600000613     C*
056700000830     C* CALCOLA LA DATA CORRENTE
056800091110     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
056900091110     C                   eval      datcor = %dec(%date() : *ISO)
057000000830     C*
057100000613     C                   ENDSR
057200000613     C***
