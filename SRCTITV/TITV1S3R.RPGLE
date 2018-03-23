000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
001500060725     D tisi95ds      e ds
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
003400000830     D*
003500990908
003600060307     D*------------------
003700060307     D* DS REPERIMENTO NUMERATORE
003800060307     D*------------------
003900060529     D trul33ds      e ds                  inz
004000060307     D*------------------
004100060307     D* DS ARCHITETTURA
004200060307     D*------------------
004300060307     D kpjba         e ds                  inz
004400060307     D*------------------
004500010201
004600091006
004700091006     D*------------------
004800091006     D* LINKING A DEFINIZIONI ESTERNE
004900091006     D*------------------
005000091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005100091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005200091006
005300010201
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005900000613     C*
006000010202     C* Effettuo la chiamata al CLLE preposto
006100040506     C                   call(e)   'TITVVTC'
006200010202     C                   parm                    parccm
006300010202     C                   parm                    parmbr
006400010202     C                   parm      '2'           paropz
006500050201     C*
006600050201     C* Effettuo lancio TISI95 solo x chiusura
006700050201     C                   CLEAR                   TISI95DS
006800050201     C                   EVAL      I95TLA = 'C'
006900050201     C                   CALL      'TISI95R'
007000050201     C                   PARM                    TISI95DS
007100000616     C*
007200000801     C
007300010201     C                   seton                                        LR
007400990908
007500000801
007600910830     C*--------------------------------------------------------
007700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007800910830     C*--------------------------------------------------------
007900040526     C     RWFILE        BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400021113     C                   if        not %open(fivabwwr)
008500021113     C                   open      fivabwwr
008600990914     C                   endif
008700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008800020305     C                   exsr      prevat
008900010201     C*
009000010202     C                   if        chkcall = '0'
009100010202     C*
009200021113     C                   if        not %open(fivatwwr)
009300021113     C                   open      fivatwwr
009400010201     C                   endif
009500990910     C*
009600010201     C                   clear                   §CTROKVB          5 0
009700020305     C                   clear                   §CTROKVT          5 0
009800000801     C                   clear                   §CTRMO            5 0
009900000801     C                   clear                   §CTRNO            5 0
010000990910     C*
010100921023     C                   DO        *HIVAL
010200990913     C*
010300990915     C                   READ      tivin00r                               70
010400050627     C                   if        vindta > *blanks
010500000613     C                   add       1             rrnum
010600000801     C*
010700000801     C                   if        *in70 = *off
010800000801     C                             and
010900000801     C                             (vinflg = *blanks
011000000801     C                              or vinflg = '0'
011100000801     C                              or vinflg = '2')
011200000801     C*
011300000801     C                   clear                   vinmsg
011400000801     C                   eval      vinflg = '1'
011500010305     C*
011600050628     C                   clear                   fivab000
011700060725     C                   clear                   fivat000
011800050628     C                   exsr      impvab                                       => carico VAB
011900060725     C                   exsr      wrivab                                       => scarico VAB
012000000905     C*
012100000905     C                   else
012200000905     C                   eval      vinflg = '1'
012300050628     C                   endif
012400000905     C                   endif
012500000905     C*
012600000905     C  N70              update    tivin000
012700000905     C*
012800991022     C  N70              ENDdo
012900010202     C*
013000010202     C                   endif
013100990910
013200990910     C* Se non ci sono record con errori ...
013300000710     C                   if        §ctrno = 0
013400990910     C* ... restituisco esito OK.
013500990921     C                   eval      wrkesito = '0'
013600990910     C                   else
013700010201     C                   if        §ctrokvb > 0
013800990921     C                   eval      wrkesito = '1'
013900000710     C                   else
014000000710     C                   eval      wrkesito = '2'
014100990910     C                   endif
014200000710     C                   endif
014300990910     C*
014400990914     C                   if        %open(tivin00r)
014500990908     C                   close     tivin00r
014600990914     C                   endif
014700021113     C                   if        %open(fivabwwr)
014800021113     C                   close     fivabwwr
014900990914     C                   endif
015000021113     C                   if        %open(fivatwwr)
015100021113     C                   close     fivatwwr
015200010201     C                   endif
015300990910     C*
015400010201     C                   if        §ctrokvb > 0
015500000724     C                             and vlrpoi <> *zeros
015600010202     C                   exsr      invio
015700990920     C                   endif
015800990920     C*
015900910830     C                   ENDSR
016000000613     C***
016100010305
016200010305     C*----------------------------------------------------*
016300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016400010305     C*----------------------------------------------------*
016500020305     C     WRIVAB        BEGSR
016600010305     C*
016700060225     C* Quindi scarico il buffer del file d testata
016800021113     C                   write     fivab000                                     => scarico il VAB
016900010305     C*
017000010305     C                   ENDSR
017100990920
017200000801     C*----------------------------------------------------*
017300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017400000801     C*----------------------------------------------------*
017500010201     C     INZVAR        BEGSR
017600000801     C*
017700040802     C                   Z-ADD     *zeros        Num5_0            5 0
017800040802     C                   MOVEL     '0'           FlgCAS            1
017900000801     C*
018000000801     C                   ENDSR
018100000801     C*----------------------------------------------------*
018200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018300000801     C*----------------------------------------------------*
018400000801     C     DEFCAM        BEGSR
018500000801     C*
018600020619     C* Imposto i valori di default...
018700091009     C                   Z-ADD     0262029       VABCCM
018800091009     C                   Z-ADD     0262029       VATCCM
018900091009     C                   Z-ADD     159           VABLNP
019000091009     C                   Z-ADD     159           VATLNP
019100040714     C                   Z-ADD     000           VABCTR
019200060725     C                   MOVEL     '1'           VABCBO
019300020619     C* ... e poi verifico se sono stati passati come parametri
019400020619     C                   IF        vlrppt > *blanks
019500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019700020619     C                   EXSR      CHKNUM
019800020619     C                   IF        PiInt=*on
019900020619     C                   Z-ADD     PiVal         VABCCM
020000020619     C                   Z-ADD     PiVal         VATCCM
020100020619     C                   ENDIF
020200040506     C                   ENDIF
020300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020500020619     C                   EXSR      CHKNUM
020600020619     C                   IF        PiInt=*on
020700020619     C                   Z-ADD     PiVal         VABLNP
020800020619     C                   Z-ADD     PiVal         VATLNP
020900040506     C                   ENDIF
021000020619     C                   ENDIF
021100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABCTR
021600040506     C                   ENDIF
021700020619     C                   ENDIF
021800020619     C                   ENDIF
021900000801     C*
022000000801     C                   ENDSR
022100000801     C*----------------------------------------------------*
022200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022300000801     C*----------------------------------------------------*
022400040823     C     IMPVAB        BEGSR
022500040823     C*
022600020305     C                   EXSR      INZVAR
022700020305     C                   EXSR      DEFCAM
022800010305     C*
022900000801     C                   Z-ADD     *zeros        errore            1 0
023000000830     C                   MOVEL     datcor        VABAAS
023100020305     C                   MOVEL     datcor        VATAAS
023200040526     C                   MOVE      datcor        VABMGS
023300060529     C                   MOVE(P)   vlrpoi        VABFGS
023400060529     C                   MOVE(P)   vlrpoi        VATFGS
023500050628     C*
023600091009     C                   EVAL      VABRSD=%trim(%subst(vindta:11:40))
023700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
023900020117     C* ==
024000091009     C                   IF        %subst(vindta:51:40) <> *blanks
024100091009     C                   EVAL      VABRSD=%trim(%subst(vindta:44:35))
024200091009     C                   ENDIF
024300091009     C                   IF        %subst(vindta:425:1) = 'S'
024400091009     C                   EVAL      VABTSP='E'
024500091009     C                   ENDIF
024600091009     C                   EVAL      VABIND=%trim(%subst(vindta:91:32))
024700091009     C                   EVAL      VABLOD=%trim(%subst(vindta:123:32))
024800091009     C                   EVAL      VABPRD=%trim(%subst(vindta:155:2))
024900091009     C                   EVAL      VABRMA=%trim(%subst(vindta:286:27))
025000091009     C                   EVAL      VABNOT=%trim(%subst(vindta:259:27))
025100091009     C                   EVAL      VABNT2=%trim(%subst(vindta:426:50))
025200091009     C                   EVAL      VABNAS=%trim(%subst(vindta:207:15))
025300091009     C* CBO
025400091006     C                   SELECT
025500091009     C                   WHEN      %subst(vindta:162:1) = 'F'
025600091009     C                   EVAL      VABCBO = '1'
025700091009     C                   WHEN      %subst(vindta:162:1) = 'C'
025800091009     C                   EVAL      VABCBO = '1'
025900091009     C                   WHEN      %subst(vindta:162:1) = 'V'
026000091009     C                   EVAL      VABCBO = '2'
026100091009     C                   WHEN      %subst(vindta:162:1) = 'D'
026200091009     C                   EVAL      VABCBO = '2'
026300091009     C                   WHEN      %subst(vindta:162:1) = 'A'
026400091009     C                   EVAL      VABCBO = '2'
026500091006     C                   ENDSL
026600060529     C* RMN
026700091009     C                   EVAL      PiStr=%trim(%subst(vindta:163:16))
026800060529     C                   EXSR      CHKNUM
026900060529     C                   IF        PiInt=*on
027000060529     C                   Z-ADD     PiVal         VABRMN
027100060529     C                   ELSE
027200060529     C                   ADD       1             errore
027300060529     C                   EVAL      vinmsg = %trimr(vinmsg)
027400060529     C                             + ' ' + 'VABRMN'
027500060529     C                   ENDIF
027600091007     C*
027700060529     C* NSP => Stacco un numeratore da AZNUM
027800060529     C                   clear                   TRUL33DS
027900060529     C                   eval      I33OPE = *zeros
028000060529     C                   eval      I33CNU = 302
028100060529     C                   eval      I33NUM = 1
028200060529     C                   movel     TRUL33DS      KPJBU
028300060529     C                   call      'TRUL33R'
028400060529     C                   parm                    KPJBA
028500060529     C                   movel     KPJBU         TRUL33DS
028600060529     C                   if        O33ERR = *zeros
028700060529     C                   z-add     O33NRF        VABNSP
028800060529     C                   z-add     O33NRF        VATNSP
028900060529     C                   else
029000060529     C                   Z-ADD     1             errore
029100060529     C                   EVAL      vinmsg = %trimr(vinmsg)
029200060529     C                             + ' ' + 'VABNSP VATNSP'
029300060529     C                   endif
029400060529     C* CAD
029500091009     C                   EVAL      PiStr=%trim(%subst(vindta:157:5))
029600010201     C                   EXSR      CHKNUM
029700010201     C                   IF        PiInt=*on
029800010201     C                   Z-ADD     PiVal         Num5_0
029900040506     C                   MOVEL(P)  Num5_0        VABCAD
030000010201     C                   ELSE
030100040506     C                   ADD       1             errore
030200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030300040506     C                             + ' ' + 'VABCAD'
030400010201     C                   ENDIF
030500091006     C*
030600040506     C* Reperisco la provincia dal CAP e dalla località
030700040526     C                   IF        VABCAD <> *blanks AND
030800040526     C                             VABPRD  = *blanks
030900040506     C                   CLEAR                   TISI95DS
031000040506     C                   EVAL      I95TCN = '3'
031100040506     C                   Z-ADD     datcor        I95DAT
031200040506     C                   EVAL      I95CAP = VABCAD
031300040506     C                   EVAL      I95LOC = VABLOD
031400050627     C                   EVAL      I95NAR = VABNZD
031500040506     C                   CALL      'TISI95R'
031600040506     C                   PARM                    TISI95DS
031700040506     C                   EVAL      VABPRD = O95PRV
031800040506     C                   ENDIF
031900091006     C* NCL
032000091009     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
032100060725     C                   EXSR      CHKNUM
032200060725     C                   IF        PiInt=*on
032300071214     C                   ADD       PiVal         VABNCL
032400060725     C                   ELSE
032500060725     C                   ADD       1             errore
032600060725     C                   EVAL      vinmsg = %trimr(vinmsg)
032700060725     C                             + ' ' + 'VABNCL'
032800060725     C                   ENDIF
032900040506     C* PKB
033000091009     C                   EVAL      PiStr=%trim(%subst(vindta:227:5))
033100010201     C                   EXSR      CHKNUM
033200010201     C                   IF        PiNum=*on
033300060725     C                   Z-ADD(H)  PiVal         VABPKB
033400010201     C                   ELSE
033500010201     C                   ADD       1             errore
033600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033700010201     C                             + ' ' + 'VABPKB'
033800010201     C                   ENDIF
033900091006     C* VLB
034000091009     C                   EVAL      PiStr=%trim(%subst(vindta:232:5))
034100091006     C                   EXSR      CHKNUM
034200091006     C                   IF        PiNum=*on
034300091007     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
034400091006     C                   Z-ADD(H)  PiVal         VABVLB
034500091006     C                   ELSE
034600091006     C                   ADD       1             errore
034700091006     C                   EVAL      vinmsg = %trimr(vinmsg)
034800091006     C                             + ' ' + 'VABVLB'
034900091006     C                   ENDIF
035000060725     C* CAS
035100091009     C                   IF        %subst(vindta:313:13)<>*all'0' AND
035200091009     C                             %subst(vindta:313:13)<>*blanks AND
035300091009     C                             %subst(vindta:313:13)<>'000000000,000' AND
035400091009     C                             %subst(vindta:313:13)<>'000000000.000'
035500071207     C                   EVAL      FlgCAS='1'
035600091006     C                   EVAL      VABVCA='EUR'
035700091009     C                   EVAL      PiStr=%trim(%subst(vindta:313:13))
035800060307     C                   EXSR      CHKNUM
035900060307     C                   IF        PiNum=*on
036000060725     C                   Z-ADD     PiVal         VABCAS
036100060307     C                   ELSE
036200060307     C                   ADD       1             errore
036300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
036400060307     C                             + ' ' + 'VABCAS'
036500060307     C                   ENDIF
036600060725     C                   ENDIF
036700060529     C*
036800091006     C* Gestione estensioni "VAT"
036900091006     C                   exsr      exeVATA
037000091007     C                   exsr      exeVATB
037100010205     C*
037200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037300040802     C                   IF        FlgCAS <> '0'
037400071207     C                   IF        VABCBO = '1'
037500071207     C                   EVAL      VABCBO = '4'
037600071207     C                   ENDIF
037700071207     C                   IF        VABCBO = '2'
037800071207     C                   EVAL      VABCBO = '6'
037900071207     C                   ENDIF
038000071207     C                   ENDIF
038100020305     C*
038200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038300011113     C                   EXSR      CHKIMPDIV
038400010202     C*
038500000801     C* Ebbene...
038600000801     C                   ADD       1             §CTRMO
038700010201     C                   IF        errore <> *zeros
038800000801     C                   ADD       1             §CTRNO
038900000801     C                   EVAL      vinflg = '2'
039000000801     C                   ELSE
039100010201     C                   ADD       1             §CTROKVB
039200000801     C                   ENDIF
039300000801     C*
039400000801     C                   ENDSR
039500050628     C*----------------------------------------------------*
039600060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039700050628     C*----------------------------------------------------*
039800071207     C     EXEVATE       BEGSR
039900071207     C*
040000091009     C***                EVAL      VATTRC = 'E'
040100091009     C***                EVAL      VATNOT = %subst(vindta:340:50)
040200091007     C                   IF        VATNOT <> *blanks
040300071207     C                   write     FIVAT000
040400071207     C                   add       1             §CTROKVT
040500091007     C                   ENDIF
040600050628     C*
040700050628     C                   ENDSR
040800091006     C*----------------------------------------------------*
040900091006     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
041000091006     C*----------------------------------------------------*
041100091006     C     EXEVATA       BEGSR
041200091006     C*
041300091009     C***                EVAL      VATTRC = 'A'
041400091009     C***                EVAL      VATNOT = %subst(vindta:340:50)
041500091006     C                   IF        VATNOT <> *blanks
041600091006     C                   write     FIVAT000
041700091006     C                   add       1             §CTROKVT
041800091006     C                   ENDIF
041900091006     C*
042000091006     C                   ENDSR
042100091007     C*----------------------------------------------------*
042200091007     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042300091007     C*----------------------------------------------------*
042400091007     C     EXEVATB       BEGSR
042500091007     C*
042600091009     C***                EVAL      VATTRC = 'B'
042700091009     C***                EVAL      VATNOT = %subst(vindta:330:10)
042800091007     C                   IF        VATNOT <> *blanks
042900091007     C                   write     FIVAT000
043000091007     C                   add       1             §CTROKVT
043100091007     C                   ENDIF
043200091007     C*
043300091007     C                   ENDSR
043400010202     C*----------------------------------------------------*
043500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043600010202     C*----------------------------------------------------*
043700020305     C     PREVAT        BEGSR
043800010202     C*
043900021113     C* Compongo il nome del membro da dare al FIVATWWR
044000010202     C                   eval      parmbr = vlrhdl
044100010202     C                   movel     'M'           parmbr
044200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
044300010202     C                   eval      paropz = '1'
044400010202     C* Effettuo la chiamata al CLLE preposto
044500040506     C                   call(e)   'TITVVTC'
044600010202     C                   parm                    parccm
044700010202     C                   parm                    parmbr
044800010202     C                   parm                    paropz
044900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045000010202     C                   if        %error
045100010202     C                   movel     '1'           chkcall
045200010202     C                   else
045300010202     C                   movel     '0'           chkcall
045400010202     C                   endif
045500010202     C*
045600010202     C                   ENDSR
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
053300011113
053400011113
053500000801
053600000801
053700990920      /TITLE Invio dei dati al punto operativo.
053800010202     C     invio         BEGSR
053900990920     C*
054000021113     C* 1° invio FIVAT
054100010201     C                   reset                   dscmz
054200010201     C                   move      vlrpoi        cmzdst
054300021113     C                   eval      cmzfld = 'FIVATWWR'
054400010201     C                   eval      cmzmbd = vlrhdl
054500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054600021009     C***                if        prmfir = *blanks
054700021113     C                   eval      cmzfla = 'FIVAT00F'
054800021113     C                   eval      cmzmba = 'FIVAT00F'
054900021009     C***                else
055000021009     C***                eval      cmzfla = prmfir
055100021009     C***                eval      cmzmba = prmfir
055200021009     C***                endif
055300010201     C                   eval      cmznrr = *zeros
055400020305     C                   move      §ctrokvt      cmznrr
055500021018     C                   eval      cmzlba = vlrfl1
055600010201     C                   call(e)   'TIS711C'
055700010201     C                   parm                    dscmz
055800010201     C                   parm      *blanks       esito
055900010205     C                   if        %error
056000010205     C                             or cmzerr = '1'
056100010205     C                             or esito  = '1'
056200010205     C                   eval      wrkesito = '3'
056300010205     C                   else
056400010201     C*
056500021113     C* 2° invio FIVAB
056600010201     C                   reset                   dscmz
056700010201     C                   move      vlrpoi        cmzdst
056800010201     C                   eval      cmzfld = vlrfou
056900010201     C                   eval      cmzmbd = vlrhdl
057000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057100021009     C***                if        prmfir = *blanks
057200021113     C                   eval      cmzfla = 'FIVAB00F'
057300021113     C                   eval      cmzmba = 'FIVAB00F'
057400021009     C***                else
057500021009     C***                eval      cmzfla = prmfir
057600021009     C***                eval      cmzmba = prmfir
057700021009     C***                endif
057800010201     C                   eval      cmznrr = *zeros
057900010201     C                   move      §ctrokvb      cmznrr
058000021018     C                   eval      cmzlba = vlrfl1
058100010201     C                   call(e)   'TIS711C'
058200010201     C                   parm                    dscmz
058300010201     C                   parm      *blanks       esito
058400010201     C                   if        %error
058500010201     C                             or cmzerr = '1'
058600010201     C                             or esito  = '1'
058700010201     C                   eval      wrkesito = '3'
058800010201     C                   endif
058900010205     C                   endif
059000990920     C*
059100000613     C                   ENDSR
059200000613     C***
059300070411
059400070411     C     *pssr         BEGSR
059500070411     C*
059600070411     C                   if        %open(tivin00r)
059700070411     C                   close     tivin00r
059800070411     C                   endif
059900070411     C                   if        %open(fivabwwr)
060000070411     C                   close     fivabwwr
060100070411     C                   endif
060200070411     C                   if        %open(fivatwwr)
060300070411     C                   close     fivatwwr
060400070411     C                   endif
060500070411     C*
060600070411     C* Effettuo la chiamata al CLLE preposto
060700070411     C                   call(e)   'TITVVTC'
060800070411     C                   parm                    parccm
060900070411     C                   parm                    parmbr
061000070411     C                   parm      '2'           paropz
061100070411     C*
061200070411     C                   eval      wrkesito = '2'
061300070411     C*
061400070411     C                   seton                                        LR
061500070411     C*
061600070411     C                   ENDSR     '*CANCL'
061700070411     C***
061800070411
061900990910
062000000613     C     *inzsr        BEGSR
062100990910     C*
062200990910     C     *entry        plist
062300990920     C                   parm                    tivlrds
062400990921     C                   parm      wrkesito      esito
062500000724     C                   parm                    prmlit
062600000710     C                   parm                    prmfir
062700000613     C*
062800000830     C* CALCOLA LA DATA CORRENTE
062900000830     C                   time                    wn14             14 0
063000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063100000830     C                   z-add     wn8           g08dat
063200000830     C                   z-add     *zeros        g08inv
063300000830     C                   movel     '0'           g08err
063400000830     C                   call      'XSRDA8'
063500000830     C                   parm                    wlbda8
063600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063700000830     C*
063800000613     C                   ENDSR
063900000613     C***
