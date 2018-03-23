000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300120312
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
025500110207     C                   EVAL      PiStr=%trim(%subst(vindta:167:10))
025600060202     C                   EXSR      CHKNUM
025700060202     C                   IF        PiInt=*on
025800060202     C                   Z-ADD     PiVal         VABRMN
025900060202     C                   ELSE
026000060202     C                   ADD       1             errore
026100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
026200060202     C                             + ' ' + 'VABRMN'
026300060202     C                   ENDIF
026400110207     C* ......VABNCL
026500110207     C                   EVAL      PiStr=%trim(%subst(vindta:96:4))
026600100714     C                   EXSR      CHKNUM
026700100714     C                   IF        PiInt=*on
026800110211     C                   Z-ADD     PiVal         VABNCL
026900100714     C                   ELSE
027000100714     C                   ADD       1             errore
027100100714     C                   EVAL      vinmsg = %trimr(vinmsg)
027200100714     C                             + ' ' + 'VABNCL'
027300100714     C                   ENDIF
027400060202     C* ......VABPKB
027500110207     C                   EVAL      PiStr=%trim(%subst(vindta:108:8))
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
028600110207     C                   EVAL      PiStr=%trim(%subst(vindta:100:8))
028700100722     C                   EXSR      CHKNUM
028800100722     C                   IF        PiNum=*on
028900110207     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
029000100722     C                   Z-ADD(H)  PiVal         VABVLB
029100100722     C                   ELSE
029200100722     C                   ADD       1             errore
029300100722     C                   EVAL      vinmsg = %trimr(vinmsg)
029400100722     C                             + ' ' + 'VABVLB'
029500100722     C                   ENDIF
029600110211     C* ......VABRMA
029700110211     C                   EVAL      VABRMA=%trim(%subst(vindta:167:10))
029800060202     C* ......VABRSD
029900110207     C                   EVAL      VABRSD=%trim(%subst(vindta:4:30))
030000100714     C* ......VABIND
030100110207     C                   EVAL      VABIND=%trim(%subst(vindta:34:25))
030200060202     C* ......VABLOD
030300110207     C                   EVAL      VABLOD=%trim(%subst(vindta:59:30))
030400060202     C* ......VABCAD
030500110211     C                   EVAL      VABCAD=%trim(%subst(vindta:89:5))
030600060202     C* ......VABPRD
030700110207     C                   EVAL      VABPRD=%trim(%subst(vindta:94:2))
030800070719     C* Reperisco la provincia dal CAP e dalla località
030900070719     C                   IF        VABPRD  = *blanks AND
031000070719     C                             VABCAD <> *blanks
031100070719     C                   CLEAR                   TISI95DS
031200070719     C                   EVAL      I95TCN = '3'
031300070719     C                   Z-ADD     datcor        I95DAT
031400070719     C                   EVAL      I95CAP = VABCAD
031500070719     C                   EVAL      I95LOC = VABLOD
031600070719     C                   CALL      'TISI95R'
031700070719     C                   PARM                    TISI95DS
031800070719     C                   EVAL      VABPRD = O95PRV
031900070719     C                   ENDIF
032000110207     C* VMD
032100110211     C*                  IF        %trim(%subst(vindta:116:12)) <> *blanks AND
032200110211     C*                            %trim(%subst(vindta:116:12)) <> *zeros
032300110211     C*                  EVAL      VABVAD = 'EUR'
032400110211     C*                  EVAL      PiStr=%trim(%subst(vindta:116:12))
032500110211     C*                  EXSR      CHKNUM
032600110211     C*                  IF        PiNum=*on
032700110211     C*                  EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
032800110211     C*                  Z-ADD     PiVal         VABVMD
032900110211     C*                  ELSE
033000110211     C*                  ADD       1             errore
033100110211     C*                  Z-ADD     *zeros        VABVMD
033200110211     C*                  EVAL      vinmsg = %trimr(vinmsg)
033300110211     C*                            + ' ' + 'VABVMD'
033400110211     C*                  ENDIF
033500110211     C*                  ENDIF
033600110207     C* CAS
033700110207     C                   IF        %trim(%subst(vindta:235:10)) <> *blanks AND
033800110207     C                             %trim(%subst(vindta:235:10)) <> *zeros
033900110207     C                   MOVEL     '1'           FlgCAS
034000110207     C                   EVAL      VABVCA = 'EUR'
034100110207     C                   EVAL      PiStr=%trim(%subst(vindta:235:10))
034200110207     C                   EXSR      CHKNUM
034300110207     C                   IF        PiNum=*on
034400110207     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
034500110207     C                   Z-ADD     PiVal         VABCAS
034600110207     C                   ELSE
034700110207     C                   ADD       1             errore
034800110207     C                   Z-ADD     *zeros        VABCAS
034900110207     C                   EVAL      vinmsg = %trimr(vinmsg)
035000110207     C                             + ' ' + 'VABCAS'
035100110207     C                   ENDIF
035200110207     C                   ENDIF
035300060202     C*
035400060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035500060202     C                   IF        FlgCAS <> '0'
035600060202     C                   IF        VABCBO = '1'
035700060202     C                   EVAL      VABCBO = '4'
035800060202     C                   ELSE
035900060202     C                   EVAL      VABCBO = '6'
036000060202     C                   ENDIF
036100060202     C                   ENDIF
036200060202     C*
036300060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036400060202     C                   EXSR      CHKIMPDIV
036500040910     C*
036600040910     C                   ENDIF
036700040910     C***
036800070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
036900110207     C                   IF        %subst(vindta:1:1) = 'D'
037000070719     C* Resetto indicatore d anomalia sul singolo record
037100070719     C                   eval      vinflg = '1'
037200100722     C* ......VATNOT
037300110207     C                   EVAL      VATNOT=%trim(%subst(vindta:2:12))
037400100722     C                   EVAL      VATTRC = 'E'
037500100722     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
037600100722     C                   IF        VATNOT <> *blanks
037700100722     C                   ADD       1             §CTROKVT
037800100722     C                   WRITE     FIVAT000
037900100722     C                   ENDIF
038000100722     C*
038100100722     C                   ENDIF
038200010202     C*
038300000801     C* Ebbene...
038400000801     C                   ADD       1             §CTRMO
038500010201     C                   IF        errore <> *zeros
038600000801     C                   ADD       1             §CTRNO
038700000801     C                   EVAL      vinflg = '2'
038800000801     C                   ELSE
038900010201     C                   ADD       1             §CTROKVB
039000000801     C                   ENDIF
039100000801     C*
039200000801     C                   ENDSR
039300010202     C*----------------------------------------------------*
039400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039500010202     C*----------------------------------------------------*
039600020305     C     PREVAT        BEGSR
039700010202     C*
039800021113     C* Compongo il nome del membro da dare al FIVATWWR
039900010202     C                   eval      parmbr = vlrhdl
040000010202     C                   movel     'M'           parmbr
040100060113     C                   eval      parccm = vlrksc
040200010202     C                   eval      paropz = '1'
040300010202     C* Effettuo la chiamata al CLLE preposto
040400040506     C                   call(e)   'TITVVTC'
040500010202     C                   parm                    parccm
040600010202     C                   parm                    parmbr
040700010202     C                   parm                    paropz
040800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040900010202     C                   if        %error
041000010202     C                   movel     '1'           chkcall
041100010202     C                   else
041200010202     C                   movel     '0'           chkcall
041300010202     C                   endif
041400010202     C*
041500010202     C                   ENDSR
041600000801     C*----------------------------------------------------*
041700000801     C*  CONTROLLO NUMERICITA' CAMPI
041800000801     C*----------------------------------------------------*
041900000801     C     CHKNUM        BEGSR
042000000801     C*
042100000801     C                   call(e)   'ISNUMERIC'
042200000801     C                   PARM                    PiStr            30
042300040714     C                   PARM      ','           PiDecChr          1
042400000801     C                   PARM      *ZEROS        PiVal            30 9
042500000801     C                   PARM      '0'           PiInt             1
042600000801     C                   PARM      '0'           PiNum             1
042700000801     C                   IF        %error
042800000801     C                   EVAL      PiInt=*off
042900000801     C                   ENDIF
043000000801     C*
043100000801     C                   ENDSR
043200000801     C***
043300000801
043400011113
043500011113     C*----------------------------------------------------*
043600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043700011113     C*----------------------------------------------------*
043800011113     C     CHKIMPDIV     BEGSR
043900011113     C*
044000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044100011113     C                   Z-ADD     *zeros        wrkDec            9 9
044200011113     C*
044300011113     C* Come prima cosa effettuo considerazioni sulla divisa
044400011113     C                   IF        vabIAS > *zeros
044500011113     C                   IF        vabVAS <> 'EUR'
044600011113     C                   EVAL      vabVAS =  'ITL'
044700011113     C                   ENDIF
044800011113     C                   ENDIF
044900011113     C*
045000011113     C                   IF        vabCAS > *zeros
045100011113     C                   IF        vabVCA <> 'EUR'
045200011113     C                   EVAL      vabVCA =  'ITL'
045300011113     C                   ENDIF
045400011113     C                   ENDIF
045500011113     C*
045600011113     C                   IF        vabVMD > *zeros
045700020305     C                   IF        vabVAD <> 'EUR'
045800011113     C                   EVAL      vabVAD =  'ITL'
045900011113     C                   ENDIF
046000011113     C                   ENDIF
046100011113     C*
046200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046300011113     C                   Z-ADD     vabIAS        wrkDec
046400011113     C                   IF        wrkDec > *zeros
046500011113     C                   IF        vabVAS = 'ITL'
046600011113     C                   EVAL      vabIAS = *zeros
046700011113     C                   ENDIF
046800011113     C                   ENDIF
046900011113     C*
047000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047100011113     C                   Z-ADD     vabCAS        wrkDec
047200011113     C                   IF        wrkDec > *zeros
047300011113     C                   IF        vabVCA = 'ITL'
047400011113     C                   EVAL      vabCAS = *zeros
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047900011113     C                   Z-ADD     vabVMD        wrkDec
048000011113     C                   IF        wrkDec > *zeros
048100011113     C                   IF        vabVAD = 'ITL'
048200011113     C                   EVAL      vabVMD = *zeros
048300011113     C                   ENDIF
048400011113     C                   ENDIF
048500011113     C*
048600011113     C                   ENDSR
048700011113     C***
048800011113
048900011113
049000000801
049100000801
049200990920      /TITLE Invio dei dati al punto operativo.
049300010202     C     invio         BEGSR
049400990920     C*
049500021113     C* 1° invio FIVAT
049600010201     C                   reset                   dscmz
049700010201     C                   move      vlrpoi        cmzdst
049800021113     C                   eval      cmzfld = 'FIVATWWR'
049900010201     C                   eval      cmzmbd = vlrhdl
050000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050100021009     C***                if        prmfir = *blanks
050200021113     C                   eval      cmzfla = 'FIVAT00F'
050300021113     C                   eval      cmzmba = 'FIVAT00F'
050400021009     C***                else
050500021009     C***                eval      cmzfla = prmfir
050600021009     C***                eval      cmzmba = prmfir
050700021009     C***                endif
050800010201     C                   eval      cmznrr = *zeros
050900020305     C                   move      §ctrokvt      cmznrr
051000021018     C                   eval      cmzlba = vlrfl1
051100010201     C                   call(e)   'TIS711C'
051200010201     C                   parm                    dscmz
051300010201     C                   parm      *blanks       esito
051400010205     C                   if        %error
051500010205     C                             or cmzerr = '1'
051600010205     C                             or esito  = '1'
051700010205     C                   eval      wrkesito = '3'
051800010205     C                   else
051900010201     C*
052000021113     C* 2° invio FIVAB
052100010201     C                   reset                   dscmz
052200010201     C                   move      vlrpoi        cmzdst
052300010201     C                   eval      cmzfld = vlrfou
052400010201     C                   eval      cmzmbd = vlrhdl
052500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052600021009     C***                if        prmfir = *blanks
052700021113     C                   eval      cmzfla = 'FIVAB00F'
052800021113     C                   eval      cmzmba = 'FIVAB00F'
052900021009     C***                else
053000021009     C***                eval      cmzfla = prmfir
053100021009     C***                eval      cmzmba = prmfir
053200021009     C***                endif
053300010201     C                   eval      cmznrr = *zeros
053400010201     C                   move      §ctrokvb      cmznrr
053500021018     C                   eval      cmzlba = vlrfl1
053600010201     C                   call(e)   'TIS711C'
053700010201     C                   parm                    dscmz
053800010201     C                   parm      *blanks       esito
053900010201     C                   if        %error
054000010201     C                             or cmzerr = '1'
054100010201     C                             or esito  = '1'
054200010201     C                   eval      wrkesito = '3'
054300010201     C                   endif
054400010205     C                   endif
054500990920     C*
054600000613     C                   ENDSR
054700000613     C***
054800070411
054900070411     C     *pssr         BEGSR
055000070411     C*
055100070411     C                   if        %open(tivin00r)
055200070411     C                   close     tivin00r
055300070411     C                   endif
055400070411     C                   if        %open(fivabwwr)
055500070411     C                   close     fivabwwr
055600070411     C                   endif
055700070411     C                   if        %open(fivatwwr)
055800070411     C                   close     fivatwwr
055900070411     C                   endif
056000070411     C*
056100070411     C* Effettuo la chiamata al CLLE preposto
056200070411     C                   call(e)   'TITVVTC'
056300070411     C                   parm                    parccm
056400070411     C                   parm                    parmbr
056500070411     C                   parm      '2'           paropz
056600070411     C*
056700070411     C                   eval      wrkesito = '2'
056800070411     C*
056900070411     C                   seton                                        LR
057000070411     C*
057100070411     C                   ENDSR     '*CANCL'
057200070411     C***
057300070411
057400990910
057500000613     C     *inzsr        BEGSR
057600990910     C*
057700990910     C     *entry        plist
057800990920     C                   parm                    tivlrds
057900990921     C                   parm      wrkesito      esito
058000000724     C                   parm                    prmlit
058100000710     C                   parm                    prmfir
058200000613     C*
058300000830     C* CALCOLA LA DATA CORRENTE
058400100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058500100722     C                   eval      datcor = %dec(%date() : *ISO)
058600000830     C*
058700000613     C                   ENDSR
058800000613     C***
