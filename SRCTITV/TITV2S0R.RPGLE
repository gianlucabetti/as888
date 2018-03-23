000100101025      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600101025     FEDIVABwr  O    E             DISK    usropn
000700101025     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600060725     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600140308     D wNumColloDA     s              7  0 INZ(*zeros)
002700140308     D wNumColloCOR    s              7  0 INZ(*zeros)
002800140308     D savVINDTA       s                   INZ(*blanks) like(vindta)
002900990908
003000060307     D*------------------
003100060307     D* DS REPERIMENTO NUMERATORE
003200060307     D*------------------
003300060529     D trul33ds      e ds                  inz
003400060307     D*------------------
003500060307     D* DS ARCHITETTURA
003600060307     D*------------------
003700060307     D kpjba         e ds                  inz
003800060307     D*------------------
003900010201
004000091006
004100091006     D*------------------
004200091006     D* LINKING A DEFINIZIONI ESTERNE
004300091006     D*------------------
004400091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004500091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004600091006
004700010201
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100000613     C*
005200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005300000613     C*
005400010202     C* Effettuo la chiamata al CLLE preposto
005500101025     C                   call(e)   'TITVEVTC'
005600010202     C                   parm                    parccm
005700010202     C                   parm                    parmbr
005800010202     C                   parm      '2'           paropz
005900050201     C*
006000050201     C* Effettuo lancio TISI95 solo x chiusura
006100050201     C                   CLEAR                   TISI95DS
006200050201     C                   EVAL      I95TLA = 'C'
006300050201     C                   CALL      'TISI95R'
006400050201     C                   PARM                    TISI95DS
006500000616     C*
006600000801     C
006700010201     C                   seton                                        LR
006800000801
006900910830     C*--------------------------------------------------------
007000101025     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007100910830     C*--------------------------------------------------------
007200040526     C     RWFILE        BEGSR
007300990910     C*
007400990914     C                   if        not %open(tivin00r)
007500990908     C                   open      tivin00r
007600990914     C                   endif
007700101025     C                   if        not %open(edivabwr)
007800101025     C                   open      edivabwr
007900990914     C                   endif
008000101025     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008100020305     C                   exsr      prevat
008200010201     C*
008300010202     C                   if        chkcall = '0'
008400010202     C*
008500101025     C                   if        not %open(edivatwr)
008600101025     C                   open      edivatwr
008700010201     C                   endif
008800990910     C*
008900010201     C                   clear                   §CTROKVB          5 0
009000020305     C                   clear                   §CTROKVT          5 0
009100000801     C                   clear                   §CTRMO            5 0
009200000801     C                   clear                   §CTRNO            5 0
009300990910     C*
009400921023     C                   DO        *HIVAL
009500990913     C*
009600990915     C                   READ      tivin00r                               70
009700050627     C                   if        vindta > *blanks
009800000613     C                   add       1             rrnum
009900000801     C*
010000000801     C                   if        *in70 = *off
010100000801     C                             and
010200000801     C                             (vinflg = *blanks
010300000801     C                              or vinflg = '0'
010400000801     C                              or vinflg = '2')
010500000801     C*
010600000801     C                   clear                   vinmsg
010700000801     C                   eval      vinflg = '1'
010800010305     C*
010900140308 xxx C* Forzatura "speciale" x gestione "strano" UNICODE
011000140308     C                   if        rrnum = 1
011100140308     C                   eval      savVINDTA = vindta
011200140509     C***                eval      vindta = ' ' + vindta
011300140308     C                   endif
011400140308     C*
011500101025     C                   clear                   edivab00
011600101025     C                   clear                   edivat00
011700050628     C                   exsr      impvab                                       => carico VAB
011800060725     C                   exsr      wrivab                                       => scarico VAB
011900140308     C*
012000140308 xxx C* Forzatura "speciale" x gestione "strano" UNICODE
012100140308     C                   if        rrnum = 1
012200140308     C                   eval      vindta = savVINDTA
012300140308     C                   endif
012400000905     C*
012500000905     C                   else
012600000905     C                   eval      vinflg = '1'
012700050628     C                   endif
012800000905     C                   endif
012900000905     C*
013000000905     C  N70              update    tivin000
013100000905     C*
013200991022     C  N70              ENDdo
013300010202     C*
013400010202     C                   endif
013500990910
013600990910     C* Se non ci sono record con errori ...
013700000710     C                   if        §ctrno = 0
013800990910     C* ... restituisco esito OK.
013900990921     C                   eval      wrkesito = '0'
014000990910     C                   else
014100010201     C                   if        §ctrokvb > 0
014200990921     C                   eval      wrkesito = '1'
014300000710     C                   else
014400000710     C                   eval      wrkesito = '2'
014500990910     C                   endif
014600000710     C                   endif
014700990910     C*
014800990914     C                   if        %open(tivin00r)
014900990908     C                   close     tivin00r
015000990914     C                   endif
015100101025     C                   if        %open(edivabwr)
015200101025     C                   close     edivabwr
015300990914     C                   endif
015400101025     C                   if        %open(edivatwr)
015500101025     C                   close     edivatwr
015600010201     C                   endif
015700990910     C*
015800010201     C                   if        §ctrokvb > 0
015900000724     C                             and vlrpoi <> *zeros
016000010202     C                   exsr      invio
016100990920     C                   endif
016200990920     C*
016300910830     C                   ENDSR
016400000613     C***
016500010305
016600010305     C*----------------------------------------------------*
016700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016800010305     C*----------------------------------------------------*
016900020305     C     WRIVAB        BEGSR
017000101025     C*
017100101025     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
017200140308     C                   EVAL      VABCMR = %char(datcor) + ' ' + %char(oracor)
017300101025     C                   EVAL      VABDCM = DATCOR
017400101025     C                   EVAL      VABDTS = DATCOR
017500101025     C                   EVAL      VABHMS = ORACOR
017600101025     C                   EVAL      VABCNT = 1
017700010305     C*
017800060225     C* Quindi scarico il buffer del file d testata
017900101025     C                   write     edivab00                                     => scarico il VAB
018000010305     C*
018100010305     C                   ENDSR
018200101025
018300101025     C*----------------------------------------------------*
018400101025     C*  SCARICAMENTO BUFFER RECORDS VAT
018500101025     C*----------------------------------------------------*
018600101025     C     WRIVAT        BEGSR
018700101025     C*
018800101025     C                   if        vatnot <> *blanks
018900101025     C*
019000101025     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
019100140308     C                   EVAL      VATCMR = %char(datcor) + ' ' + %char(oracor)
019200101025     C                   EVAL      VATCNT = 1
019300101025     C*
019400101025     C* Quindi scarico il buffer del file d dettaglio
019500101025     C                   write     edivat00
019600101025     C                   add       1             §CTROKVT
019700101025     C                   endif
019800101025     C*
019900101025     C                   ENDSR
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
021100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021200000801     C*----------------------------------------------------*
021300000801     C     DEFCAM        BEGSR
021400000801     C*
021500020619     C* Imposto i valori di default...
021600140308     C                   Z-ADD     1662318       VABCCM
021700140308     C                   Z-ADD     1662318       VATCCM
021800140308     C                   Z-ADD     166           VABLNP
021900140308     C                   Z-ADD     166           VATLNP
022000140308     C                   Z-ADD     300           VABCTR
022100060529     C                   MOVEL     '7Q'          VABCTM
022200060725     C                   MOVEL     '1'           VABCBO
022300020619     C* ... e poi verifico se sono stati passati come parametri
022400020619     C                   IF        vlrppt > *blanks
022500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022700020619     C                   EXSR      CHKNUM
022800020619     C                   IF        PiInt=*on
022900020619     C                   Z-ADD     PiVal         VABCCM
023000020619     C                   Z-ADD     PiVal         VATCCM
023100020619     C                   ENDIF
023200040506     C                   ENDIF
023300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABLNP
023800020619     C                   Z-ADD     PiVal         VATLNP
023900040506     C                   ENDIF
024000020619     C                   ENDIF
024100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024300020619     C                   EXSR      CHKNUM
024400020619     C                   IF        PiInt=*on
024500020619     C                   Z-ADD     PiVal         VABCTR
024600040506     C                   ENDIF
024700020619     C                   ENDIF
024800020619     C                   ENDIF
024900000801     C*
025000000801     C                   ENDSR
025100000801     C*----------------------------------------------------*
025200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025300000801     C*----------------------------------------------------*
025400040823     C     IMPVAB        BEGSR
025500040823     C*
025600020305     C                   EXSR      INZVAR
025700020305     C                   EXSR      DEFCAM
025800010305     C*
025900000801     C                   Z-ADD     *zeros        errore            1 0
026000000830     C                   MOVEL     datcor        VABAAS
026100020305     C                   MOVEL     datcor        VATAAS
026200040526     C                   MOVE      datcor        VABMGS
026300060529     C                   MOVE(P)   vlrpoi        VABFGS
026400060529     C                   MOVE(P)   vlrpoi        VATFGS
026500050628     C*
026600140308     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
026700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026900020117     C* ==
027000140308     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
027100140308     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
027200140308     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
027300140308     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
027400140308     C                   EVAL      VABRMA=%trim(%subst(vindta:95:15))
027500140308     C                   EVAL      VABNAS=%trim(%subst(vindta:95:15))
027600060529     C* RMN
027700140308     C                   EVAL      PiStr=%trim(%subst(vindta:80+7:15-7))
027800060529     C                   EXSR      CHKNUM
027900060529     C                   IF        PiInt=*on
028000060529     C                   Z-ADD     PiVal         VABRMN
028100060529     C                   ELSE
028200060529     C                   ADD       1             errore
028300060529     C                   EVAL      vinmsg = %trimr(vinmsg)
028400060529     C                             + ' ' + 'VABRMN'
028500060529     C                   ENDIF
028600060529     C* NSP => Stacco un numeratore da AZNUM
028700060529     C                   clear                   TRUL33DS
028800060529     C                   eval      I33OPE = *zeros
028900060529     C                   eval      I33CNU = 302
029000060529     C                   eval      I33NUM = 1
029100060529     C                   movel     TRUL33DS      KPJBU
029200060529     C                   call      'TRUL33R'
029300060529     C                   parm                    KPJBA
029400060529     C                   movel     KPJBU         TRUL33DS
029500060529     C                   if        O33ERR = *zeros
029600060529     C                   z-add     O33NRF        VABNSP
029700060529     C                   z-add     O33NRF        VATNSP
029800060529     C                   else
029900060529     C                   Z-ADD     1             errore
030000060529     C                   EVAL      vinmsg = %trimr(vinmsg)
030100060529     C                             + ' ' + 'VABNSP VATNSP'
030200060529     C                   endif
030300060529     C* CAD
030400140308     C                   EVAL      PiStr=%trim(%subst(vindta:316:7))
030500010201     C                   EXSR      CHKNUM
030600010201     C                   IF        PiInt=*on
030700010201     C                   Z-ADD     PiVal         Num5_0
030800040506     C                   MOVEL(P)  Num5_0        VABCAD
030900010201     C                   ELSE
031000040506     C                   ADD       1             errore
031100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031200040506     C                             + ' ' + 'VABCAD'
031300010201     C                   ENDIF
031400091006     C*
031500040506     C* Reperisco la provincia dal CAP e dalla località
031600140308     C                   IF        VABNZD  = *blanks AND
031700140308     C                             VABLOD <> *blanks AND
031800140308     C                             VABCAD <> *blanks AND
031900040526     C                             VABPRD  = *blanks
032000040506     C                   CLEAR                   TISI95DS
032100040506     C                   EVAL      I95TCN = '3'
032200040506     C                   Z-ADD     datcor        I95DAT
032300040506     C                   EVAL      I95CAP = VABCAD
032400040506     C                   EVAL      I95LOC = VABLOD
032500050627     C                   EVAL      I95NAR = VABNZD
032600040506     C                   CALL      'TISI95R'
032700040506     C                   PARM                    TISI95DS
032800040506     C                   EVAL      VABPRD = O95PRV
032900040506     C                   ENDIF
033000091006     C* NCL
033100140308     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
033200060725     C                   EXSR      CHKNUM
033300060725     C                   IF        PiInt=*on
033400140308     C                   Z-ADD     PiVal         VABNCL
033500060725     C                   ELSE
033600060725     C                   ADD       1             errore
033700060725     C                   EVAL      vinmsg = %trimr(vinmsg)
033800060725     C                             + ' ' + 'VABNCL'
033900060725     C                   ENDIF
034000040506     C* PKB
034100140308     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
034200010201     C                   EXSR      CHKNUM
034300010201     C                   IF        PiNum=*on
034400140308     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 dec.
034500060725     C                   Z-ADD(H)  PiVal         VABPKB
034600010201     C                   ELSE
034700010201     C                   ADD       1             errore
034800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034900010201     C                             + ' ' + 'VABPKB'
035000010201     C                   ENDIF
035100060529     C*
035200091006     C* Gestione estensioni "VAT"
035300060725     C                   exsr      exeVATE
035400140308     C***                exsr      exeVATB
035500010205     C*
035600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035700040802     C                   IF        FlgCAS <> '0'
035800071207     C                   IF        VABCBO = '1'
035900071207     C                   EVAL      VABCBO = '4'
036000071207     C                   ENDIF
036100071207     C                   IF        VABCBO = '2'
036200071207     C                   EVAL      VABCBO = '6'
036300071207     C                   ENDIF
036400071207     C                   ENDIF
036500020305     C*
036600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036700011113     C                   EXSR      CHKIMPDIV
036800010202     C*
036900000801     C* Ebbene...
037000000801     C                   ADD       1             §CTRMO
037100010201     C                   IF        errore <> *zeros
037200000801     C                   ADD       1             §CTRNO
037300000801     C                   EVAL      vinflg = '2'
037400000801     C                   ELSE
037500010201     C                   ADD       1             §CTROKVB
037600000801     C                   ENDIF
037700000801     C*
037800000801     C                   ENDSR
037900050628     C*----------------------------------------------------*
038000060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
038100050628     C*----------------------------------------------------*
038200071207     C     EXEVATE       BEGSR
038300140308     C*
038400140308     C* Sviluppo i "chi sono" cliente dai segnacolli sequenziali su file
038500140308     C                   EVAL      PiStr=%trim(%subst(vindta:371:7))
038600140308     C                   EXSR      CHKNUM
038700140308     C                   IF        PiInt=*on
038800140308     C                   Z-ADD     PiVal         wNumColloDA
038900140308     C                   ELSE
039000140308     C                   ADD       1             errore
039100140308     C                   EVAL      vinmsg = %trimr(vinmsg)
039200140308     C                             + ' ' + 'VATNOT_E'
039300140308     C                   ENDIF
039400140308     C*
039500140308     C                   EVAL      wNumColloCOR = wNumColloDA
039600140308     C                   DOW       wNumColloCOR < wNumColloDA + VABNCL
039700140308     C                   EVAL      VATNOT = 'JJD00029105300' +
039800140308     C                                      %trim(%editc(wNumColloCOR:'X'))
039900140308     C                   EVAL      wNumColloCOR = wNumColloCOR + 1
040000140308     C                   EVAL      VATTRC = 'E'
040100101025     C                   exsr      wrivat
040200071207     C                   ENDDO
040300050628     C*
040400050628     C                   ENDSR
040500091006     C*----------------------------------------------------*
040600091109     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040700091006     C*----------------------------------------------------*
040800091109     C     EXEVATB       BEGSR
040900091006     C*
041000091109     C                   EVAL      VATTRC = 'B'
041100091006     C                   EVAL      VATNOT = %subst(vindta:428:10)
041200091006     C                   IF        VATNOT <> *blanks
041300101025     C                   exsr      wrivat
041400091006     C                   ENDIF
041500091006     C*
041600091006     C                   ENDSR
041700010202     C*----------------------------------------------------*
041800101025     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
041900010202     C*----------------------------------------------------*
042000020305     C     PREVAT        BEGSR
042100010202     C*
042200101025     C* Compongo il nome del membro da dare al EDIVATWR
042300010202     C                   eval      parmbr = vlrhdl
042400010202     C                   movel     'M'           parmbr
042500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
042600010202     C                   eval      paropz = '1'
042700010202     C* Effettuo la chiamata al CLLE preposto
042800101025     C                   call(e)   'TITVEVTC'
042900010202     C                   parm                    parccm
043000010202     C                   parm                    parmbr
043100010202     C                   parm                    paropz
043200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043300010202     C                   if        %error
043400010202     C                   movel     '1'           chkcall
043500010202     C                   else
043600010202     C                   movel     '0'           chkcall
043700010202     C                   endif
043800010202     C*
043900010202     C                   ENDSR
044000000801     C*----------------------------------------------------*
044100000801     C*  CONTROLLO NUMERICITA' CAMPI
044200000801     C*----------------------------------------------------*
044300091006     C     CHKNUM        BEGSR
044400091006     C*
044500091006     C                   IF        PiDecChr = *blanks
044600091006     C                   EVAL      PiDecChr = ','
044700091006     C                   ENDIF
044800091006     C*
044900091006     C                   callp(e)  UBISNUM_Check(PiStr
045000091006     C                                          :PiDecChr
045100091006     C                                          :PiVal
045200091006     C                                          :PiNum
045300091006     C                                          :PiInt)
045400091006     C*
045500091006     C                   IF        %error
045600091006     C                   EVAL      PiInt=*off
045700091006     C                   ENDIF
045800000801     C*
045900000801     C                   ENDSR
046000000801     C***
046100000801
046200011113
046300011113     C*----------------------------------------------------*
046400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046500011113     C*----------------------------------------------------*
046600011113     C     CHKIMPDIV     BEGSR
046700011113     C*
046800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046900011113     C                   Z-ADD     *zeros        wrkDec            9 9
047000011113     C*
047100011113     C* Come prima cosa effettuo considerazioni sulla divisa
047200011113     C                   IF        vabIAS > *zeros
047300011113     C                   IF        vabVAS <> 'EUR'
047400011113     C                   EVAL      vabVAS =  'ITL'
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C                   IF        vabCAS > *zeros
047900011113     C                   IF        vabVCA <> 'EUR'
048000011113     C                   EVAL      vabVCA =  'ITL'
048100011113     C                   ENDIF
048200011113     C                   ENDIF
048300011113     C*
048400011113     C                   IF        vabVMD > *zeros
048500020305     C                   IF        vabVAD <> 'EUR'
048600011113     C                   EVAL      vabVAD =  'ITL'
048700011113     C                   ENDIF
048800011113     C                   ENDIF
048900011113     C*
049000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049100011113     C                   Z-ADD     vabIAS        wrkDec
049200011113     C                   IF        wrkDec > *zeros
049300011113     C                   IF        vabVAS = 'ITL'
049400011113     C                   EVAL      vabIAS = *zeros
049500011113     C                   ENDIF
049600011113     C                   ENDIF
049700011113     C*
049800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049900011113     C                   Z-ADD     vabCAS        wrkDec
050000011113     C                   IF        wrkDec > *zeros
050100011113     C                   IF        vabVCA = 'ITL'
050200011113     C                   EVAL      vabCAS = *zeros
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050700011113     C                   Z-ADD     vabVMD        wrkDec
050800011113     C                   IF        wrkDec > *zeros
050900011113     C                   IF        vabVAD = 'ITL'
051000011113     C                   EVAL      vabVMD = *zeros
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C                   ENDSR
051500011113     C***
051600011113
051700011113
051800000801
051900000801
052000990920      /TITLE Invio dei dati al punto operativo.
052100010202     C     invio         BEGSR
052200990920     C*
052300021113     C* 1° invio FIVAT
052400010201     C                   reset                   dscmz
052500010201     C                   move      vlrpoi        cmzdst
052600101025     C                   eval      cmzfld = 'EDIVATWR'
052700010201     C                   eval      cmzmbd = vlrhdl
052800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052900021009     C***                if        prmfir = *blanks
053000101025     C                   eval      cmzfla = 'EDIVAT0F'
053100101025     C                   eval      cmzmba = 'EDIVAT0F'
053200021009     C***                else
053300021009     C***                eval      cmzfla = prmfir
053400021009     C***                eval      cmzmba = prmfir
053500021009     C***                endif
053600010201     C                   eval      cmznrr = *zeros
053700020305     C                   move      §ctrokvt      cmznrr
053800021018     C                   eval      cmzlba = vlrfl1
053900010201     C                   call(e)   'TIS711C'
054000010201     C                   parm                    dscmz
054100010201     C                   parm      *blanks       esito
054200010205     C                   if        %error
054300010205     C                             or cmzerr = '1'
054400010205     C                             or esito  = '1'
054500010205     C                   eval      wrkesito = '3'
054600010205     C                   else
054700010201     C*
054800021113     C* 2° invio FIVAB
054900010201     C                   reset                   dscmz
055000010201     C                   move      vlrpoi        cmzdst
055100010201     C                   eval      cmzfld = vlrfou
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500101025     C                   eval      cmzfla = 'EDIVAB0F'
055600101025     C                   eval      cmzmba = 'EDIVAB0F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200010201     C                   move      §ctrokvb      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010201     C                   if        %error
056800010201     C                             or cmzerr = '1'
056900010201     C                             or esito  = '1'
057000010201     C                   eval      wrkesito = '3'
057100010201     C                   endif
057200010205     C                   endif
057300990920     C*
057400000613     C                   ENDSR
057500000613     C***
057600070411
057700070411     C     *pssr         BEGSR
057800070411     C*
057900070411     C                   if        %open(tivin00r)
058000070411     C                   close     tivin00r
058100070411     C                   endif
058200101025     C                   if        %open(edivabwr)
058300101025     C                   close     edivabwr
058400070411     C                   endif
058500101025     C                   if        %open(edivatwr)
058600101025     C                   close     edivatwr
058700070411     C                   endif
058800070411     C*
058900070411     C* Effettuo la chiamata al CLLE preposto
059000101025     C                   call(e)   'TITVEVTC'
059100070411     C                   parm                    parccm
059200070411     C                   parm                    parmbr
059300070411     C                   parm      '2'           paropz
059400070411     C*
059500070411     C                   eval      wrkesito = '2'
059600070411     C*
059700070411     C                   seton                                        LR
059800070411     C*
059900070411     C                   ENDSR     '*CANCL'
060000070411     C***
060100070411
060200990910
060300000613     C     *inzsr        BEGSR
060400990910     C*
060500990910     C     *entry        plist
060600990920     C                   parm                    tivlrds
060700990921     C                   parm      wrkesito      esito
060800000724     C                   parm                    prmlit
060900000710     C                   parm                    prmfir
061000000613     C*
061100000830     C* CALCOLA LA DATA CORRENTE
061200000830     C                   time                    wn14             14 0
061300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061400101025     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
061500140308     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061600140308     C                   eval      datcor = %dec(%date() :*iso)
061700000830     C*
061800000613     C                   ENDSR
061900000613     C***
