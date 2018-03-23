000100121115      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500121115     FEDIVABwr  O    E             DISK    usropn
000600121115     FEDIVATwr  O    E             DISK    usropn
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
005100121115     C                   call(e)   'TITVEVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500000616     C*
005600010201     C                   seton                                        LR
005700990908
005800000801
005900910830     C*--------------------------------------------------------
006000121115     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006100910830     C*--------------------------------------------------------
006200040526     C     RWFILE        BEGSR
006300990910     C*
006400990914     C                   if        not %open(tivin00r)
006500990908     C                   open      tivin00r
006600990914     C                   endif
006700121115     C                   if        not %open(EDIVABwr)
006800121115     C                   open      EDIVABwr
006900990914     C                   endif
007000121115     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500121115     C                   if        not %open(EDIVATwr)
007600121115     C                   open      EDIVATwr
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
011500121115     C*
011600121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
011700121115     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:55:5))
011800121115     C                   EVAL      VABDCM = datcor
011900121115     C                   EVAL      VABDTS = datcor
012000121115     C                   EVAL      VABHMS = oracor
012100121115     C                   EVAL      VABCNT = 1
012200121115     C*
012300121115     C                   WRITE     EDIVAB00
012400010202     C*
012500010202     C                   endif
012600990910
012700990910     C* Se non ci sono record con errori ...
012800000710     C                   if        §ctrno = 0
012900990910     C* ... restituisco esito OK.
013000990921     C                   eval      wrkesito = '0'
013100990910     C                   else
013200010201     C                   if        §ctrokvb > 0
013300990921     C                   eval      wrkesito = '1'
013400000710     C                   else
013500000710     C                   eval      wrkesito = '2'
013600990910     C                   endif
013700000710     C                   endif
013800990910     C*
013900990914     C                   if        %open(tivin00r)
014000990908     C                   close     tivin00r
014100990914     C                   endif
014200121115     C                   if        %open(EDIVABwr)
014300121115     C                   close     EDIVABwr
014400990914     C                   endif
014500121115     C                   if        %open(EDIVATwr)
014600121115     C                   close     EDIVATwr
014700010201     C                   endif
014800990910     C*
014900010201     C                   if        §ctrokvb > 0
015000000724     C                             and vlrpoi <> *zeros
015100010202     C                   exsr      invio
015200990920     C                   endif
015300990920     C*
015400910830     C                   ENDSR
015500000613     C***
015600990920
015700000801     C*----------------------------------------------------*
015800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015900000801     C*----------------------------------------------------*
016000010201     C     INZVAR        BEGSR
016100000801     C*
016200040802     C                   Z-ADD     *zeros        Num5_0            5 0
016300040802     C                   MOVEL     '0'           FlgCAS            1
016400000801     C*
016500000801     C                   ENDSR
016600000801     C*----------------------------------------------------*
016700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016800000801     C*----------------------------------------------------*
016900000801     C     DEFCAM        BEGSR
017000000801     C*
017100121115     C                   CLEAR                   EDIVAB00
017200121115     C                   CLEAR                   EDIVAT00
017300020619     C* Imposto i valori di default...
017400121115     C                   Z-ADD     1660780       VABCCM
017500121115     C                   Z-ADD     1660780       VATCCM
017600121115     C                   Z-ADD     166           VABLNP
017700121115     C                   Z-ADD     166           VATLNP
017800070531     C                   Z-ADD     000           VABCTR
017900070531     C                   MOVEL     '7Q'          VABCTM
018000040823     C                   MOVEL     '1'           VABCBO
018100020619     C* ... e poi verifico se sono stati passati come parametri
018200020619     C                   IF        vlrppt > *blanks
018300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018500020619     C                   EXSR      CHKNUM
018600020619     C                   IF        PiInt=*on
018700020619     C                   Z-ADD     PiVal         VABCCM
018800020619     C                   Z-ADD     PiVal         VATCCM
018900020619     C                   ENDIF
019000040506     C                   ENDIF
019100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
019200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019300020619     C                   EXSR      CHKNUM
019400020619     C                   IF        PiInt=*on
019500020619     C                   Z-ADD     PiVal         VABLNP
019600020619     C                   Z-ADD     PiVal         VATLNP
019700040506     C                   ENDIF
019800020619     C                   ENDIF
019900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020100020619     C                   EXSR      CHKNUM
020200020619     C                   IF        PiInt=*on
020300020619     C                   Z-ADD     PiVal         VABCTR
020400040506     C                   ENDIF
020500020619     C                   ENDIF
020600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
020700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020800060202     C                   ENDIF
020900020619     C                   ENDIF
021000000801     C*
021100000801     C                   ENDSR
021200000801     C*----------------------------------------------------*
021300040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
021400000801     C*----------------------------------------------------*
021500040910     C     IMPVABVAT     BEGSR
021600040910     C*
021700040910     C* Traduzione relativa ai tipi record del file del cliente
021800121115     C*
021900121115     C***
022000121115     C* ...tipo record 'EBESTD' (testata documento)
022100121115     C                   IF        %subst(vindta:1:6) = 'EBESTD'
022200121115     C*
022300121115     C* Se nn primo giro => scarico il buffer precedente
022400121115     C                   if        wGiro = 1
022500121115     C                   eval      wGiro = 2
022600121115     C                   else
022700121115     C*
022800121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
022900121115     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:55:5))
023000121115     C                   EVAL      VABDCM = datcor
023100121115     C                   EVAL      VABDTS = datcor
023200121115     C                   EVAL      VABHMS = oracor
023300121115     C                   EVAL      VABCNT = 1
023400121115     C*
023500121115     C                   WRITE     EDIVAB00
023600121115     C                   endif
023700121115     C*
023800121115     C* Resetto indicatore d anomalia sul singolo record
023900121115     C                   eval      vinflg = '1'
024000121115     C* ......inizializzazioni iniziali e formati record file Bartolini
024100121115     C                   EXSR      INZVAR
024200121115     C                   EXSR      DEFCAM
024300121115     C*
024400121115     C                   Z-ADD     *zeros        errore            1 0
024500121115     C                   MOVEL     datcor        VABAAS
024600121115     C                   MOVEL     datcor        VATAAS
024700121115     C                   MOVE      datcor        VABMGS
024800071210     C                   MOVE(P)   vlrpoi        VABFGS
024900071210     C                   MOVE(P)   vlrpoi        VATFGS
025000071210     C* ......VABNSP/VATNSP
025100071210     C* NSP => Stacco un numeratore da AZNUM
025200071210     C                   clear                   TRUL33DS
025300071210     C                   eval      I33OPE = *zeros
025400071210     C                   eval      I33CNU = 302
025500071210     C                   eval      I33NUM = 1
025600071210     C                   movel     TRUL33DS      KPJBU
025700071210     C                   call      'TRUL33R'
025800071210     C                   parm                    KPJBA
025900071210     C                   movel     KPJBU         TRUL33DS
026000071210     C                   if        O33ERR = *zeros
026100071210     C                   z-add     O33NRF        VABNSP
026200071210     C                   z-add     O33NRF        VATNSP
026300071210     C                   else
026400071210     C                   Z-ADD     1             errore
026500071210     C                   EVAL      vinmsg = %trimr(vinmsg)
026600071210     C                             + ' ' + 'VABNSP VATNSP'
026700071210     C                   endif
026800060202     C* ......VABRMN
026900121115     C                   EVAL      PiStr=%trim(%subst(vindta:25:15))
027000060202     C                   EXSR      CHKNUM
027100060202     C                   IF        PiInt=*on
027200060202     C                   Z-ADD     PiVal         VABRMN
027300121115     C                   MOVEL     VABRMN        VABRMA
027400060202     C                   ELSE
027500060202     C                   ADD       1             errore
027600060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027700121115     C                             + ' ' + 'VABRMN VABRMA'
027800060202     C                   ENDIF
027900121115     C* ......VABRSD
028000121115     C                   EVAL      VABRSD=%trim(%subst(vindta:56:35))
028100121116     C* ......VABIND
028200121116     C                   EVAL      VABIND=%trim(%subst(vindta:91:35))
028300121116     C* ......VABRD2
028400121116     C                   EVAL      VABRD2=%trim(%subst(vindta:126:35))
028500121115     C* ......VABCAD
028600121115     C                   EVAL      VABCAD=%trim(%subst(vindta:231:9))
028700121115     C* ......VABLOD
028800121115     C                   EVAL      VABLOD=%trim(%subst(vindta:240:35))
028900110207     C* ......VABNCL
029000121115     C                   EVAL      PiStr=%trim(%subst(vindta:313:5))
029100100714     C                   EXSR      CHKNUM
029200100714     C                   IF        PiInt=*on
029300110211     C                   Z-ADD     PiVal         VABNCL
029400100714     C                   ELSE
029500100714     C                   ADD       1             errore
029600100714     C                   EVAL      vinmsg = %trimr(vinmsg)
029700100714     C                             + ' ' + 'VABNCL'
029800100714     C                   ENDIF
029900060202     C* ......VABPKB
030000121115     C                   EVAL      PiStr=%trim(%subst(vindta:318:9))
030100060202     C                   EXSR      CHKNUM
030200060202     C                   IF        PiNum=*on
030300121115     C                   EVAL      PiVal = PiVal / 1000                         gest. 3 dec.
030400070726     C                   Z-ADD(H)  PiVal         VABPKB
030500060202     C                   ELSE
030600060202     C                   ADD       1             errore
030700060202     C                   EVAL      vinmsg = %trimr(vinmsg)
030800060202     C                             + ' ' + 'VABPKB'
030900060202     C                   ENDIF
031000070719     C* Reperisco la provincia dal CAP e dalla località
031100070719     C                   IF        VABPRD  = *blanks AND
031200070719     C                             VABCAD <> *blanks
031300070719     C                   CLEAR                   TISI95DS
031400070719     C                   EVAL      I95TCN = '3'
031500070719     C                   Z-ADD     datcor        I95DAT
031600070719     C                   EVAL      I95CAP = VABCAD
031700070719     C                   EVAL      I95LOC = VABLOD
031800070719     C                   CALL      'TISI95R'
031900070719     C                   PARM                    TISI95DS
032000070719     C                   EVAL      VABPRD = O95PRV
032100070719     C                   ENDIF
032200060202     C*
032300060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032400060202     C                   IF        FlgCAS <> '0'
032500060202     C                   IF        VABCBO = '1'
032600060202     C                   EVAL      VABCBO = '4'
032700060202     C                   ELSE
032800060202     C                   EVAL      VABCBO = '6'
032900060202     C                   ENDIF
033000060202     C                   ENDIF
033100060202     C*
033200060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033300121115     C                   EXSR      CHKIMPDIV
033400040910     C*
033500040910     C                   ENDIF
033600040910     C***
033700121115     C* ...tipo record 'EBCSPC' (dettaglio barcode segnacooli)
033800121115     C                   IF        %subst(vindta:1:6) = 'EBCSPC'
033900070719     C* Resetto indicatore d anomalia sul singolo record
034000070719     C                   eval      vinflg = '1'
034100100722     C* ......VATNOT
034200121116     C                   EVAL      VATNOT=%trim(%subst(vindta:49:8))
034300100722     C                   EVAL      VATTRC = 'E'
034400121115     C* x ogni tipo record 'EBCSPC' devo scaricare il buffer del file FIVAT
034500100722     C                   IF        VATNOT <> *blanks
034600100722     C                   ADD       1             §CTROKVT
034700121115     C*
034800121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
034900121115     C                   EVAL      VATCMR=%trim(%subst(vlrmsg:55:5))
035000121115     C                   EVAL      VATCNT = 1
035100121115     C*
035200121115     C                   WRITE     EDIVAT00
035300100722     C                   ENDIF
035400100722     C*
035500100722     C                   ENDIF
035600010202     C*
035700000801     C* Ebbene...
035800000801     C                   ADD       1             §CTRMO
035900010201     C                   IF        errore <> *zeros
036000000801     C                   ADD       1             §CTRNO
036100000801     C                   EVAL      vinflg = '2'
036200000801     C                   ELSE
036300010201     C                   ADD       1             §CTROKVB
036400000801     C                   ENDIF
036500000801     C*
036600000801     C                   ENDSR
036700010202     C*----------------------------------------------------*
036800121115     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
036900010202     C*----------------------------------------------------*
037000020305     C     PREVAT        BEGSR
037100010202     C*
037200121115     C* Compongo il nome del membro da dare al EDIVATWR
037300010202     C                   eval      parmbr = vlrhdl
037400010202     C                   movel     'M'           parmbr
037500060113     C                   eval      parccm = vlrksc
037600010202     C                   eval      paropz = '1'
037700010202     C* Effettuo la chiamata al CLLE preposto
037800121115     C                   call(e)   'TITVEVTC'
037900010202     C                   parm                    parccm
038000010202     C                   parm                    parmbr
038100010202     C                   parm                    paropz
038200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038300010202     C                   if        %error
038400010202     C                   movel     '1'           chkcall
038500010202     C                   else
038600010202     C                   movel     '0'           chkcall
038700010202     C                   endif
038800010202     C*
038900010202     C                   ENDSR
039000000801     C*----------------------------------------------------*
039100000801     C*  CONTROLLO NUMERICITA' CAMPI
039200000801     C*----------------------------------------------------*
039300000801     C     CHKNUM        BEGSR
039400000801     C*
039500000801     C                   call(e)   'ISNUMERIC'
039600000801     C                   PARM                    PiStr            30
039700040714     C                   PARM      ','           PiDecChr          1
039800000801     C                   PARM      *ZEROS        PiVal            30 9
039900000801     C                   PARM      '0'           PiInt             1
040000000801     C                   PARM      '0'           PiNum             1
040100000801     C                   IF        %error
040200000801     C                   EVAL      PiInt=*off
040300000801     C                   ENDIF
040400000801     C*
040500000801     C                   ENDSR
040600000801     C***
040700000801
040800011113
040900011113     C*----------------------------------------------------*
041000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041100011113     C*----------------------------------------------------*
041200011113     C     CHKIMPDIV     BEGSR
041300011113     C*
041400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041500011113     C                   Z-ADD     *zeros        wrkDec            9 9
041600011113     C*
041700011113     C* Come prima cosa effettuo considerazioni sulla divisa
041800011113     C                   IF        vabIAS > *zeros
041900011113     C                   IF        vabVAS <> 'EUR'
042000011113     C                   EVAL      vabVAS =  'ITL'
042100011113     C                   ENDIF
042200011113     C                   ENDIF
042300011113     C*
042400011113     C                   IF        vabCAS > *zeros
042500011113     C                   IF        vabVCA <> 'EUR'
042600011113     C                   EVAL      vabVCA =  'ITL'
042700011113     C                   ENDIF
042800011113     C                   ENDIF
042900011113     C*
043000011113     C                   IF        vabVMD > *zeros
043100020305     C                   IF        vabVAD <> 'EUR'
043200011113     C                   EVAL      vabVAD =  'ITL'
043300011113     C                   ENDIF
043400011113     C                   ENDIF
043500011113     C*
043600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043700011113     C                   Z-ADD     vabIAS        wrkDec
043800011113     C                   IF        wrkDec > *zeros
043900011113     C                   IF        vabVAS = 'ITL'
044000011113     C                   EVAL      vabIAS = *zeros
044100011113     C                   ENDIF
044200011113     C                   ENDIF
044300011113     C*
044400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044500011113     C                   Z-ADD     vabCAS        wrkDec
044600011113     C                   IF        wrkDec > *zeros
044700011113     C                   IF        vabVCA = 'ITL'
044800011113     C                   EVAL      vabCAS = *zeros
044900011113     C                   ENDIF
045000011113     C                   ENDIF
045100011113     C*
045200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045300011113     C                   Z-ADD     vabVMD        wrkDec
045400011113     C                   IF        wrkDec > *zeros
045500011113     C                   IF        vabVAD = 'ITL'
045600011113     C                   EVAL      vabVMD = *zeros
045700011113     C                   ENDIF
045800011113     C                   ENDIF
045900011113     C*
046000011113     C                   ENDSR
046100011113     C***
046200011113
046300011113
046400000801
046500000801
046600990920      /TITLE Invio dei dati al punto operativo.
046700010202     C     invio         BEGSR
046800990920     C*
046900021113     C* 1° invio FIVAT
047000010201     C                   reset                   dscmz
047100010201     C                   move      vlrpoi        cmzdst
047200121115     C                   eval      cmzfld = 'EDIVATWR'
047300010201     C                   eval      cmzmbd = vlrhdl
047400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047500021009     C***                if        prmfir = *blanks
047600121115     C                   eval      cmzfla = 'EDIVAT0F'
047700121115     C                   eval      cmzmba = 'EDIVAT0F'
047800021009     C***                else
047900021009     C***                eval      cmzfla = prmfir
048000021009     C***                eval      cmzmba = prmfir
048100021009     C***                endif
048200010201     C                   eval      cmznrr = *zeros
048300020305     C                   move      §ctrokvt      cmznrr
048400021018     C                   eval      cmzlba = vlrfl1
048500010201     C                   call(e)   'TIS711C'
048600010201     C                   parm                    dscmz
048700010201     C                   parm      *blanks       esito
048800010205     C                   if        %error
048900010205     C                             or cmzerr = '1'
049000010205     C                             or esito  = '1'
049100010205     C                   eval      wrkesito = '3'
049200010205     C                   else
049300010201     C*
049400021113     C* 2° invio FIVAB
049500010201     C                   reset                   dscmz
049600010201     C                   move      vlrpoi        cmzdst
049700010201     C                   eval      cmzfld = vlrfou
049800010201     C                   eval      cmzmbd = vlrhdl
049900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050000021009     C***                if        prmfir = *blanks
050100121115     C                   eval      cmzfla = 'EDIVAB0F'
050200121115     C                   eval      cmzmba = 'EDIVAB0F'
050300021009     C***                else
050400021009     C***                eval      cmzfla = prmfir
050500021009     C***                eval      cmzmba = prmfir
050600021009     C***                endif
050700010201     C                   eval      cmznrr = *zeros
050800010201     C                   move      §ctrokvb      cmznrr
050900021018     C                   eval      cmzlba = vlrfl1
051000010201     C                   call(e)   'TIS711C'
051100010201     C                   parm                    dscmz
051200010201     C                   parm      *blanks       esito
051300010201     C                   if        %error
051400010201     C                             or cmzerr = '1'
051500010201     C                             or esito  = '1'
051600010201     C                   eval      wrkesito = '3'
051700010201     C                   endif
051800010205     C                   endif
051900990920     C*
052000000613     C                   ENDSR
052100000613     C***
052200070411
052300070411     C     *pssr         BEGSR
052400070411     C*
052500070411     C                   if        %open(tivin00r)
052600070411     C                   close     tivin00r
052700070411     C                   endif
052800121115     C                   if        %open(EDIVABwr)
052900121115     C                   close     EDIVABwr
053000070411     C                   endif
053100121115     C                   if        %open(EDIVATwr)
053200121115     C                   close     EDIVATwr
053300070411     C                   endif
053400070411     C*
053500070411     C* Effettuo la chiamata al CLLE preposto
053600121115     C                   call(e)   'TITVEVTC'
053700070411     C                   parm                    parccm
053800070411     C                   parm                    parmbr
053900070411     C                   parm      '2'           paropz
054000070411     C*
054100070411     C                   eval      wrkesito = '2'
054200070411     C*
054300070411     C                   seton                                        LR
054400070411     C*
054500070411     C                   ENDSR     '*CANCL'
054600070411     C***
054700070411
054800990910
054900000613     C     *inzsr        BEGSR
055000990910     C*
055100990910     C     *entry        plist
055200990920     C                   parm                    tivlrds
055300990921     C                   parm      wrkesito      esito
055400000724     C                   parm                    prmlit
055500000710     C                   parm                    prmfir
055600000613     C*
055700000830     C* CALCOLA LA DATA CORRENTE
055800100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
055900100722     C                   eval      datcor = %dec(%date() : *ISO)
056000121115     C                   time                    wn14             14 0
056100121115     C                   movel     wn14          oracor            6 0          *ORA
056200000830     C*
056300000613     C                   ENDSR
056400000613     C***
