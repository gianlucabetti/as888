000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
001500070719     D tisi95ds      e ds
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
002600041025     D*------------------
002700041025     D* DS REPERIMENTO NUMERATORE
002800041025     D*------------------
002900041025     D trul33ds      e ds                  inz
003000041025     D*------------------
003100041025     D* DS ARCHITETTURA
003200041025     D*------------------
003300041025     D kpjba         e ds                  inz
003400041025     D*------------------
003500990908
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300070719     C*
004400070719     C* Esegue lancio TISI95R solo x chiusura
004500070719     C                   CLEAR                   TISI95DS
004600070719     C                   EVAL      I95TLA = 'C'
004700070719     C                   CALL      'TISI95R'
004800070719     C                   PARM                    TISI95DS
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100040506     C                   call(e)   'TITVVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500000616     C*
005600010201     C                   seton                                        LR
005700990908
005800000801
005900910830     C*--------------------------------------------------------
006000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006100910830     C*--------------------------------------------------------
006200040526     C     RWFILE        BEGSR
006300990910     C*
006400990914     C                   if        not %open(tivin00r)
006500990908     C                   open      tivin00r
006600990914     C                   endif
006700021113     C                   if        not %open(fivabwwr)
006800021113     C                   open      fivabwwr
006900990914     C                   endif
007000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500021113     C                   if        not %open(fivatwwr)
007600021113     C                   open      fivatwwr
007700010201     C                   endif
007800990910     C*
007900010201     C                   clear                   §CTROKVB          5 0
008000020305     C                   clear                   §CTROKVT          5 0
008100000801     C                   clear                   §CTRMO            5 0
008200000801     C                   clear                   §CTRNO            5 0
008300100729     C*
008400100729     C                   z-add     1             wGiro             1 0
008500040910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900040910     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000040910     C*
010100040910     C* Eseguo routine d traduzione
010200040910     C                   exsr      impvabvat
010300040802     C*
010400010305     C                   endif
010500000905     C*
010600000905     C                   else
010700000905     C                   eval      vinflg = '1'
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200991022     C  N70              ENDdo
011300100722     C*
011400100722     C* Scarico i buffer testata ancora "in canna"
011500100722     C                   WRITE     FIVAB000
011600010202     C*
011700010202     C                   endif
011800990910
011900990910     C* Se non ci sono record con errori ...
012000000710     C                   if        §ctrno = 0
012100990910     C* ... restituisco esito OK.
012200990921     C                   eval      wrkesito = '0'
012300990910     C                   else
012400010201     C                   if        §ctrokvb > 0
012500990921     C                   eval      wrkesito = '1'
012600000710     C                   else
012700000710     C                   eval      wrkesito = '2'
012800990910     C                   endif
012900000710     C                   endif
013000990910     C*
013100990914     C                   if        %open(tivin00r)
013200990908     C                   close     tivin00r
013300990914     C                   endif
013400021113     C                   if        %open(fivabwwr)
013500021113     C                   close     fivabwwr
013600990914     C                   endif
013700021113     C                   if        %open(fivatwwr)
013800021113     C                   close     fivatwwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   if        §ctrokvb > 0
014200000724     C                             and vlrpoi <> *zeros
014300010202     C                   exsr      invio
014400990920     C                   endif
014500990920     C*
014600910830     C                   ENDSR
014700000613     C***
014800990920
014900000801     C*----------------------------------------------------*
015000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015100000801     C*----------------------------------------------------*
015200010201     C     INZVAR        BEGSR
015300000801     C*
015400040802     C                   Z-ADD     *zeros        Num5_0            5 0
015500040802     C                   MOVEL     '0'           FlgCAS            1
015600000801     C*
015700000801     C                   ENDSR
015800000801     C*----------------------------------------------------*
015900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016000000801     C*----------------------------------------------------*
016100000801     C     DEFCAM        BEGSR
016200000801     C*
016300021113     C                   CLEAR                   FIVAB000
016400040802     C                   CLEAR                   FIVAT000
016500020619     C* Imposto i valori di default...
016600110207     C                   Z-ADD     0023098       VABCCM
016700110207     C                   Z-ADD     0023098       VATCCM
016800110207     C                   Z-ADD     002           VABLNP
016900110207     C                   Z-ADD     002           VATLNP
017000070531     C                   Z-ADD     000           VABCTR
017100070531     C                   MOVEL     '7Q'          VABCTM
017200040823     C                   MOVEL     '1'           VABCBO
017300020619     C* ... e poi verifico se sono stati passati come parametri
017400020619     C                   IF        vlrppt > *blanks
017500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017700020619     C                   EXSR      CHKNUM
017800020619     C                   IF        PiInt=*on
017900020619     C                   Z-ADD     PiVal         VABCCM
018000020619     C                   Z-ADD     PiVal         VATCCM
018100020619     C                   ENDIF
018200040506     C                   ENDIF
018300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018500020619     C                   EXSR      CHKNUM
018600020619     C                   IF        PiInt=*on
018700020619     C                   Z-ADD     PiVal         VABLNP
018800020619     C                   Z-ADD     PiVal         VATLNP
018900040506     C                   ENDIF
019000020619     C                   ENDIF
019100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019300020619     C                   EXSR      CHKNUM
019400020619     C                   IF        PiInt=*on
019500020619     C                   Z-ADD     PiVal         VABCTR
019600040506     C                   ENDIF
019700020619     C                   ENDIF
019800060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
019900060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020000060202     C                   ENDIF
020100020619     C                   ENDIF
020200000801     C*
020300000801     C                   ENDSR
020400000801     C*----------------------------------------------------*
020500040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020600000801     C*----------------------------------------------------*
020700040910     C     IMPVABVAT     BEGSR
020800040910     C*
020900040910     C* Traduzione relativa ai tipi record del file del cliente
021000040910     C*
021100071210     C*
021200071210     C***
021300110207     C* ...tipo record 'T' (testata)
021400110207     C                   IF        %subst(vindta:1:1) = 'T'
021500100722     C*
021600100722     C* Se nn primo giro => scarico il buffer precedente
021700100722     C                   if        wGiro = 1
021800100722     C                   eval      wGiro = 2
021900100722     C                   else
022000100722     C                   WRITE     FIVAB000
022100100722     C                   endif
022200100722     C*
022300071210     C* Resetto indicatore d anomalia sul singolo record
022400071210     C                   eval      vinflg = '1'
022500071210     C* ......inizializzazioni iniziali e formati record file Bartolini
022600071210     C                   EXSR      INZVAR
022700071210     C                   EXSR      DEFCAM
022800071210     C*
022900071210     C                   Z-ADD     *zeros        errore            1 0
023000071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023100071210     C                   MOVEL     datcor        VABAAS
023200071210     C                   MOVEL     datcor        VATAAS
023300071210     C                   MOVE      datcor        VABMGS
023400071210     C                   MOVE(P)   vlrpoi        VABFGS
023500071210     C                   MOVE(P)   vlrpoi        VATFGS
023600071210     C* ......VABNSP/VATNSP
023700071210     C* NSP => Stacco un numeratore da AZNUM
023800071210     C                   clear                   TRUL33DS
023900071210     C                   eval      I33OPE = *zeros
024000071210     C                   eval      I33CNU = 302
024100071210     C                   eval      I33NUM = 1
024200071210     C                   movel     TRUL33DS      KPJBU
024300071210     C                   call      'TRUL33R'
024400071210     C                   parm                    KPJBA
024500071210     C                   movel     KPJBU         TRUL33DS
024600071210     C                   if        O33ERR = *zeros
024700071210     C                   z-add     O33NRF        VABNSP
024800071210     C                   z-add     O33NRF        VATNSP
024900071210     C                   else
025000071210     C                   Z-ADD     1             errore
025100071210     C                   EVAL      vinmsg = %trimr(vinmsg)
025200071210     C                             + ' ' + 'VABNSP VATNSP'
025300071210     C                   endif
025400060202     C* ......VABRMN
025500120223     C                   EVAL      PiStr=%trim(%subst(vindta:242:10))
025600060202     C                   EXSR      CHKNUM
025700060202     C                   IF        PiInt=*on
025800060202     C                   Z-ADD     PiVal         VABRMN
025900060202     C                   ELSE
026000060202     C                   ADD       1             errore
026100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
026200060202     C                             + ' ' + 'VABRMN'
026300060202     C                   ENDIF
026400110207     C* ......VABNCL
026500120223     C                   EVAL      PiStr=%trim(%subst(vindta:171:4))
026600100714     C                   EXSR      CHKNUM
026700100714     C                   IF        PiInt=*on
026800110211     C                   Z-ADD     PiVal         VABNCL
026900100714     C                   ELSE
027000100714     C                   ADD       1             errore
027100100714     C                   EVAL      vinmsg = %trimr(vinmsg)
027200100714     C                             + ' ' + 'VABNCL'
027300100714     C                   ENDIF
027400060202     C* ......VABPKB
027500120223     C                   EVAL      PiStr=%trim(%subst(vindta:185:8))
027600060202     C                   EXSR      CHKNUM
027700060202     C                   IF        PiNum=*on
027800110207     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
027900070726     C                   Z-ADD(H)  PiVal         VABPKB
028000060202     C                   ELSE
028100060202     C                   ADD       1             errore
028200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028300060202     C                             + ' ' + 'VABPKB'
028400060202     C                   ENDIF
028500071210     C* ......VABVLB
028600120223     C                   EVAL      PiStr=%trim(%subst(vindta:175:8))
028700100722     C                   EXSR      CHKNUM
028800100722     C                   IF        PiNum=*on
028900110207     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
029000100722     C                   Z-ADD(H)  PiVal         VABVLB
029100100722     C                   ELSE
029200100722     C                   ADD       1             errore
029300100722     C                   EVAL      vinmsg = %trimr(vinmsg)
029400100722     C                             + ' ' + 'VABVLB'
029500100722     C                   ENDIF
029600130916     C* ......VABNOT + VABNT2
029700130916     C                   MOVEL     *blanks       wNOTE            70
029800130916     C                   EVAL      wNOTE='SE MERCE DANNEGGIATA O SCONDIZ. '+
029900130916     C                             'RENDERE MITTENTE - NO CONSEGNE PARZIALI'
030000130916     C                   EVAL      VABNOT = %subst(wNOTE:01:35)
030100130916     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
030200110211     C* ......VABRMA
030300120223     C                   EVAL      VABRMA=%trim(%subst(vindta:242:10))
030400060202     C* ......VABRSD
030500110207     C                   EVAL      VABRSD=%trim(%subst(vindta:4:30))
030600100714     C* ......VABIND
030700120223     C                   EVAL      VABIND=%trim(%subst(vindta:34:100))
030800060202     C* ......VABLOD
030900120223     C                   EVAL      VABLOD=%trim(%subst(vindta:134:30))
031000060202     C* ......VABCAD
031100120223     C                   EVAL      VABCAD=%trim(%subst(vindta:164:5))
031200060202     C* ......VABPRD
031300120223     C                   EVAL      VABPRD=%trim(%subst(vindta:169:2))
031400070719     C* Reperisco la provincia dal CAP e dalla località
031500070719     C                   IF        VABPRD  = *blanks AND
031600070719     C                             VABCAD <> *blanks
031700070719     C                   CLEAR                   TISI95DS
031800070719     C                   EVAL      I95TCN = '3'
031900070719     C                   Z-ADD     datcor        I95DAT
032000070719     C                   EVAL      I95CAP = VABCAD
032100070719     C                   EVAL      I95LOC = VABLOD
032200070719     C                   CALL      'TISI95R'
032300070719     C                   PARM                    TISI95DS
032400070719     C                   EVAL      VABPRD = O95PRV
032500070719     C                   ENDIF
032600110207     C* VMD
032700110211     C*                  IF        %trim(%subst(vindta:116:12)) <> *blanks AND
032800110211     C*                            %trim(%subst(vindta:116:12)) <> *zeros
032900110211     C*                  EVAL      VABVAD = 'EUR'
033000110211     C*                  EVAL      PiStr=%trim(%subst(vindta:116:12))
033100110211     C*                  EXSR      CHKNUM
033200110211     C*                  IF        PiNum=*on
033300110211     C*                  EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
033400110211     C*                  Z-ADD     PiVal         VABVMD
033500110211     C*                  ELSE
033600110211     C*                  ADD       1             errore
033700110211     C*                  Z-ADD     *zeros        VABVMD
033800110211     C*                  EVAL      vinmsg = %trimr(vinmsg)
033900110211     C*                            + ' ' + 'VABVMD'
034000110211     C*                  ENDIF
034100110211     C*                  ENDIF
034200110207     C* CAS
034300120223     C                   IF        %trim(%subst(vindta:310:10)) <> *blanks AND
034400120223     C                             %trim(%subst(vindta:310:10)) <> *zeros
034500110207     C                   MOVEL     '1'           FlgCAS
034600110207     C                   EVAL      VABVCA = 'EUR'
034700120223     C                   EVAL      PiStr=%trim(%subst(vindta:310:10))
034800110207     C                   EXSR      CHKNUM
034900110207     C                   IF        PiNum=*on
035000110207     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
035100110207     C                   Z-ADD     PiVal         VABCAS
035200110207     C                   ELSE
035300110207     C                   ADD       1             errore
035400110207     C                   Z-ADD     *zeros        VABCAS
035500110207     C                   EVAL      vinmsg = %trimr(vinmsg)
035600110207     C                             + ' ' + 'VABCAS'
035700110207     C                   ENDIF
035800110207     C                   ENDIF
035900060202     C*
036000060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036100060202     C                   IF        FlgCAS <> '0'
036200060202     C                   IF        VABCBO = '1'
036300060202     C                   EVAL      VABCBO = '4'
036400060202     C                   ELSE
036500060202     C                   EVAL      VABCBO = '6'
036600060202     C                   ENDIF
036700060202     C                   ENDIF
036800060202     C*
036900060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037000060202     C                   EXSR      CHKIMPDIV
037100040910     C*
037200040910     C                   ENDIF
037300040910     C***
037400070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
037500110207     C                   IF        %subst(vindta:1:1) = 'D'
037600070719     C* Resetto indicatore d anomalia sul singolo record
037700070719     C                   eval      vinflg = '1'
037800100722     C* ......VATNOT
037900110207     C                   EVAL      VATNOT=%trim(%subst(vindta:2:12))
038000100722     C                   EVAL      VATTRC = 'E'
038100100722     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
038200100722     C                   IF        VATNOT <> *blanks
038300100722     C                   ADD       1             §CTROKVT
038400100722     C                   WRITE     FIVAT000
038500100722     C                   ENDIF
038600100722     C*
038700100722     C                   ENDIF
038800010202     C*
038900000801     C* Ebbene...
039000000801     C                   ADD       1             §CTRMO
039100010201     C                   IF        errore <> *zeros
039200000801     C                   ADD       1             §CTRNO
039300000801     C                   EVAL      vinflg = '2'
039400000801     C                   ELSE
039500010201     C                   ADD       1             §CTROKVB
039600000801     C                   ENDIF
039700000801     C*
039800000801     C                   ENDSR
039900010202     C*----------------------------------------------------*
040000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040100010202     C*----------------------------------------------------*
040200020305     C     PREVAT        BEGSR
040300010202     C*
040400021113     C* Compongo il nome del membro da dare al FIVATWWR
040500010202     C                   eval      parmbr = vlrhdl
040600010202     C                   movel     'M'           parmbr
040700060113     C                   eval      parccm = vlrksc
040800010202     C                   eval      paropz = '1'
040900010202     C* Effettuo la chiamata al CLLE preposto
041000040506     C                   call(e)   'TITVVTC'
041100010202     C                   parm                    parccm
041200010202     C                   parm                    parmbr
041300010202     C                   parm                    paropz
041400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041500010202     C                   if        %error
041600010202     C                   movel     '1'           chkcall
041700010202     C                   else
041800010202     C                   movel     '0'           chkcall
041900010202     C                   endif
042000010202     C*
042100010202     C                   ENDSR
042200000801     C*----------------------------------------------------*
042300000801     C*  CONTROLLO NUMERICITA' CAMPI
042400000801     C*----------------------------------------------------*
042500000801     C     CHKNUM        BEGSR
042600000801     C*
042700000801     C                   call(e)   'ISNUMERIC'
042800000801     C                   PARM                    PiStr            30
042900040714     C                   PARM      ','           PiDecChr          1
043000000801     C                   PARM      *ZEROS        PiVal            30 9
043100000801     C                   PARM      '0'           PiInt             1
043200000801     C                   PARM      '0'           PiNum             1
043300000801     C                   IF        %error
043400000801     C                   EVAL      PiInt=*off
043500000801     C                   ENDIF
043600000801     C*
043700000801     C                   ENDSR
043800000801     C***
043900000801
044000011113
044100011113     C*----------------------------------------------------*
044200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044300011113     C*----------------------------------------------------*
044400011113     C     CHKIMPDIV     BEGSR
044500011113     C*
044600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044700011113     C                   Z-ADD     *zeros        wrkDec            9 9
044800011113     C*
044900011113     C* Come prima cosa effettuo considerazioni sulla divisa
045000011113     C                   IF        vabIAS > *zeros
045100011113     C                   IF        vabVAS <> 'EUR'
045200011113     C                   EVAL      vabVAS =  'ITL'
045300011113     C                   ENDIF
045400011113     C                   ENDIF
045500011113     C*
045600011113     C                   IF        vabCAS > *zeros
045700011113     C                   IF        vabVCA <> 'EUR'
045800011113     C                   EVAL      vabVCA =  'ITL'
045900011113     C                   ENDIF
046000011113     C                   ENDIF
046100011113     C*
046200011113     C                   IF        vabVMD > *zeros
046300020305     C                   IF        vabVAD <> 'EUR'
046400011113     C                   EVAL      vabVAD =  'ITL'
046500011113     C                   ENDIF
046600011113     C                   ENDIF
046700011113     C*
046800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046900011113     C                   Z-ADD     vabIAS        wrkDec
047000011113     C                   IF        wrkDec > *zeros
047100011113     C                   IF        vabVAS = 'ITL'
047200011113     C                   EVAL      vabIAS = *zeros
047300011113     C                   ENDIF
047400011113     C                   ENDIF
047500011113     C*
047600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047700011113     C                   Z-ADD     vabCAS        wrkDec
047800011113     C                   IF        wrkDec > *zeros
047900011113     C                   IF        vabVCA = 'ITL'
048000011113     C                   EVAL      vabCAS = *zeros
048100011113     C                   ENDIF
048200011113     C                   ENDIF
048300011113     C*
048400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048500011113     C                   Z-ADD     vabVMD        wrkDec
048600011113     C                   IF        wrkDec > *zeros
048700011113     C                   IF        vabVAD = 'ITL'
048800011113     C                   EVAL      vabVMD = *zeros
048900011113     C                   ENDIF
049000011113     C                   ENDIF
049100011113     C*
049200011113     C                   ENDSR
049300011113     C***
049400011113
049500011113
049600000801
049700000801
049800990920      /TITLE Invio dei dati al punto operativo.
049900010202     C     invio         BEGSR
050000990920     C*
050100021113     C* 1° invio FIVAT
050200010201     C                   reset                   dscmz
050300010201     C                   move      vlrpoi        cmzdst
050400021113     C                   eval      cmzfld = 'FIVATWWR'
050500010201     C                   eval      cmzmbd = vlrhdl
050600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050700021009     C***                if        prmfir = *blanks
050800021113     C                   eval      cmzfla = 'FIVAT00F'
050900021113     C                   eval      cmzmba = 'FIVAT00F'
051000021009     C***                else
051100021009     C***                eval      cmzfla = prmfir
051200021009     C***                eval      cmzmba = prmfir
051300021009     C***                endif
051400010201     C                   eval      cmznrr = *zeros
051500020305     C                   move      §ctrokvt      cmznrr
051600021018     C                   eval      cmzlba = vlrfl1
051700010201     C                   call(e)   'TIS711C'
051800010201     C                   parm                    dscmz
051900010201     C                   parm      *blanks       esito
052000010205     C                   if        %error
052100010205     C                             or cmzerr = '1'
052200010205     C                             or esito  = '1'
052300010205     C                   eval      wrkesito = '3'
052400010205     C                   else
052500010201     C*
052600021113     C* 2° invio FIVAB
052700010201     C                   reset                   dscmz
052800010201     C                   move      vlrpoi        cmzdst
052900010201     C                   eval      cmzfld = vlrfou
053000010201     C                   eval      cmzmbd = vlrhdl
053100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053200021009     C***                if        prmfir = *blanks
053300021113     C                   eval      cmzfla = 'FIVAB00F'
053400021113     C                   eval      cmzmba = 'FIVAB00F'
053500021009     C***                else
053600021009     C***                eval      cmzfla = prmfir
053700021009     C***                eval      cmzmba = prmfir
053800021009     C***                endif
053900010201     C                   eval      cmznrr = *zeros
054000010201     C                   move      §ctrokvb      cmznrr
054100021018     C                   eval      cmzlba = vlrfl1
054200010201     C                   call(e)   'TIS711C'
054300010201     C                   parm                    dscmz
054400010201     C                   parm      *blanks       esito
054500010201     C                   if        %error
054600010201     C                             or cmzerr = '1'
054700010201     C                             or esito  = '1'
054800010201     C                   eval      wrkesito = '3'
054900010201     C                   endif
055000010205     C                   endif
055100990920     C*
055200000613     C                   ENDSR
055300000613     C***
055400070411
055500070411     C     *pssr         BEGSR
055600070411     C*
055700070411     C                   if        %open(tivin00r)
055800070411     C                   close     tivin00r
055900070411     C                   endif
056000070411     C                   if        %open(fivabwwr)
056100070411     C                   close     fivabwwr
056200070411     C                   endif
056300070411     C                   if        %open(fivatwwr)
056400070411     C                   close     fivatwwr
056500070411     C                   endif
056600070411     C*
056700070411     C* Effettuo la chiamata al CLLE preposto
056800070411     C                   call(e)   'TITVVTC'
056900070411     C                   parm                    parccm
057000070411     C                   parm                    parmbr
057100070411     C                   parm      '2'           paropz
057200070411     C*
057300070411     C                   eval      wrkesito = '2'
057400070411     C*
057500070411     C                   seton                                        LR
057600070411     C*
057700070411     C                   ENDSR     '*CANCL'
057800070411     C***
057900070411
058000990910
058100000613     C     *inzsr        BEGSR
058200990910     C*
058300990910     C     *entry        plist
058400990920     C                   parm                    tivlrds
058500990921     C                   parm      wrkesito      esito
058600000724     C                   parm                    prmlit
058700000710     C                   parm                    prmfir
058800000613     C*
058900000830     C* CALCOLA LA DATA CORRENTE
059000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
059100100722     C                   eval      datcor = %dec(%date() : *ISO)
059200000830     C*
059300000613     C                   ENDSR
059400000613     C***
