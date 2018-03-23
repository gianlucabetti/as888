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
016400140821     C                   MOVEL     *blanks       wWHOIS_C         18
016500140821     C                   MOVEL     *blanks       wWHOIS_B         18
016600140821     C                   MOVEL     *blanks       wWHOIS_SAV       18
016700000801     C*
016800000801     C                   ENDSR
016900000801     C*----------------------------------------------------*
017000040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017100000801     C*----------------------------------------------------*
017200000801     C     DEFCAM        BEGSR
017300000801     C*
017400121115     C                   CLEAR                   EDIVAB00
017500121115     C                   CLEAR                   EDIVAT00
017600020619     C* Imposto i valori di default...
017700121115     C                   Z-ADD     1660780       VABCCM
017800121115     C                   Z-ADD     1660780       VATCCM
017900121115     C                   Z-ADD     166           VABLNP
018000121115     C                   Z-ADD     166           VATLNP
018100070531     C                   Z-ADD     000           VABCTR
018200070531     C                   MOVEL     '7Q'          VABCTM
018300040823     C                   MOVEL     '1'           VABCBO
018400020619     C* ... e poi verifico se sono stati passati come parametri
018500020619     C                   IF        vlrppt > *blanks
018600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018800020619     C                   EXSR      CHKNUM
018900020619     C                   IF        PiInt=*on
019000020619     C                   Z-ADD     PiVal         VABCCM
019100020619     C                   Z-ADD     PiVal         VATCCM
019200020619     C                   ENDIF
019300040506     C                   ENDIF
019400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
019500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019600020619     C                   EXSR      CHKNUM
019700020619     C                   IF        PiInt=*on
019800020619     C                   Z-ADD     PiVal         VABLNP
019900020619     C                   Z-ADD     PiVal         VATLNP
020000040506     C                   ENDIF
020100020619     C                   ENDIF
020200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020400020619     C                   EXSR      CHKNUM
020500020619     C                   IF        PiInt=*on
020600020619     C                   Z-ADD     PiVal         VABCTR
020700040506     C                   ENDIF
020800020619     C                   ENDIF
020900060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
021000060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
021100060202     C                   ENDIF
021200020619     C                   ENDIF
021300000801     C*
021400000801     C                   ENDSR
021500000801     C*----------------------------------------------------*
021600040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
021700000801     C*----------------------------------------------------*
021800040910     C     IMPVABVAT     BEGSR
021900040910     C*
022000040910     C* Traduzione relativa ai tipi record del file del cliente
022100121115     C*
022200121115     C***
022300121115     C* ...tipo record 'EBESTD' (testata documento)
022400121115     C                   IF        %subst(vindta:1:6) = 'EBESTD'
022500121115     C*
022600121115     C* Se nn primo giro => scarico il buffer precedente
022700121115     C                   if        wGiro = 1
022800121115     C                   eval      wGiro = 2
022900121115     C                   else
023000121115     C*
023100121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
023200121115     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:55:5))
023300121115     C                   EVAL      VABDCM = datcor
023400121115     C                   EVAL      VABDTS = datcor
023500121115     C                   EVAL      VABHMS = oracor
023600121115     C                   EVAL      VABCNT = 1
023700121115     C*
023800121115     C                   WRITE     EDIVAB00
023900121115     C                   endif
024000121115     C*
024100121115     C* Resetto indicatore d anomalia sul singolo record
024200121115     C                   eval      vinflg = '1'
024300121115     C* ......inizializzazioni iniziali e formati record file Bartolini
024400121115     C                   EXSR      INZVAR
024500121115     C                   EXSR      DEFCAM
024600121115     C*
024700121115     C                   Z-ADD     *zeros        errore            1 0
024800121115     C                   MOVEL     datcor        VABAAS
024900121115     C                   MOVEL     datcor        VATAAS
025000121115     C                   MOVE      datcor        VABMGS
025100071210     C                   MOVE(P)   vlrpoi        VABFGS
025200071210     C                   MOVE(P)   vlrpoi        VATFGS
025300071210     C* ......VABNSP/VATNSP
025400071210     C* NSP => Stacco un numeratore da AZNUM
025500071210     C                   clear                   TRUL33DS
025600071210     C                   eval      I33OPE = *zeros
025700071210     C                   eval      I33CNU = 302
025800071210     C                   eval      I33NUM = 1
025900071210     C                   movel     TRUL33DS      KPJBU
026000071210     C                   call      'TRUL33R'
026100071210     C                   parm                    KPJBA
026200071210     C                   movel     KPJBU         TRUL33DS
026300071210     C                   if        O33ERR = *zeros
026400071210     C                   z-add     O33NRF        VABNSP
026500071210     C                   z-add     O33NRF        VATNSP
026600071210     C                   else
026700071210     C                   Z-ADD     1             errore
026800071210     C                   EVAL      vinmsg = %trimr(vinmsg)
026900071210     C                             + ' ' + 'VABNSP VATNSP'
027000071210     C                   endif
027100060202     C* ......VABRMN
027200121115     C                   EVAL      PiStr=%trim(%subst(vindta:25:15))
027300060202     C                   EXSR      CHKNUM
027400060202     C                   IF        PiInt=*on
027500060202     C                   Z-ADD     PiVal         VABRMN
027600121115     C                   MOVEL     VABRMN        VABRMA
027700060202     C                   ELSE
027800060202     C                   ADD       1             errore
027900060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028000121115     C                             + ' ' + 'VABRMN VABRMA'
028100060202     C                   ENDIF
028200121115     C* ......VABRSD
028300121115     C                   EVAL      VABRSD=%trim(%subst(vindta:56:35))
028400121116     C* ......VABIND
028500121116     C                   EVAL      VABIND=%trim(%subst(vindta:91:35))
028600121116     C* ......VABRD2
028700121116     C                   EVAL      VABRD2=%trim(%subst(vindta:126:35))
028800121115     C* ......VABCAD
028900121115     C                   EVAL      VABCAD=%trim(%subst(vindta:231:9))
029000121115     C* ......VABLOD
029100121115     C                   EVAL      VABLOD=%trim(%subst(vindta:240:35))
029200110207     C* ......VABNCL
029300121115     C                   EVAL      PiStr=%trim(%subst(vindta:313:5))
029400100714     C                   EXSR      CHKNUM
029500100714     C                   IF        PiInt=*on
029600110211     C                   Z-ADD     PiVal         VABNCL
029700100714     C                   ELSE
029800100714     C                   ADD       1             errore
029900100714     C                   EVAL      vinmsg = %trimr(vinmsg)
030000100714     C                             + ' ' + 'VABNCL'
030100100714     C                   ENDIF
030200060202     C* ......VABPKB
030300121115     C                   EVAL      PiStr=%trim(%subst(vindta:318:9))
030400060202     C                   EXSR      CHKNUM
030500060202     C                   IF        PiNum=*on
030600121115     C                   EVAL      PiVal = PiVal / 1000                         gest. 3 dec.
030700070726     C                   Z-ADD(H)  PiVal         VABPKB
030800060202     C                   ELSE
030900060202     C                   ADD       1             errore
031000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
031100060202     C                             + ' ' + 'VABPKB'
031200060202     C                   ENDIF
031300070719     C* Reperisco la provincia dal CAP e dalla località
031400070719     C                   IF        VABPRD  = *blanks AND
031500070719     C                             VABCAD <> *blanks
031600070719     C                   CLEAR                   TISI95DS
031700070719     C                   EVAL      I95TCN = '3'
031800070719     C                   Z-ADD     datcor        I95DAT
031900070719     C                   EVAL      I95CAP = VABCAD
032000070719     C                   EVAL      I95LOC = VABLOD
032100070719     C                   CALL      'TISI95R'
032200070719     C                   PARM                    TISI95DS
032300070719     C                   EVAL      VABPRD = O95PRV
032400070719     C                   ENDIF
032500060202     C*
032600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032700060202     C                   IF        FlgCAS <> '0'
032800060202     C                   IF        VABCBO = '1'
032900060202     C                   EVAL      VABCBO = '4'
033000060202     C                   ELSE
033100060202     C                   EVAL      VABCBO = '6'
033200060202     C                   ENDIF
033300060202     C                   ENDIF
033400060202     C*
033500060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033600121115     C                   EXSR      CHKIMPDIV
033700040910     C*
033800040910     C                   ENDIF
033900040910     C***
034000140821     C* ...tipo record 'EBCSTD' (dettaglio barcode segnacooli)
034100140821     C                   IF        %subst(vindta:1:6) = 'EBCSTD'
034200070719     C* Resetto indicatore d anomalia sul singolo record
034300070719     C                   eval      vinflg = '1'
034400100722     C* ......VATNOT
034500140922     C                   MOVEL     *blanks       wDISKC_C         18
034600140922     C                   EVAL      wDISKC_C=%trim(%subst(vindta:40:18))
034700140922     C                   EVAL      wWHOIS_C=%subst(wDISKC_C:10:8)
034800140922     C                   EVAL      wWHOIS_B='00'+%trim(%subst(vindta:129:18))
034900100722     C*
035000100722     C                   ENDIF
035100140821     C***
035200140821     C* ...tipo record 'EBCSPC' (dettaglio barcode segnacooli)
035300140821     C                   IF        %subst(vindta:1:6) = 'EBCSPC'
035400140821     C* Resetto indicatore d anomalia sul singolo record
035500140821     C                   eval      vinflg = '1'
035600140821     C* ......VATNOT
035700140821     C                   IF        %trim(%subst(vindta:149:4)) = 'VPER'
035800140821     C                   EVAL      VATNOT = wWHOIS_C
035900140821     C                   ENDIF
036000140821     C                   IF        %trim(%subst(vindta:149:4)) = 'PPER'
036100140821     C                   EVAL      VATNOT = wWHOIS_B
036200140821     C                   ENDIF
036300140821     C*
036400140821     C                   IF        VATNOT = wWHOIS_SAV
036500140821     C                   EVAL      VATNOT = *blanks
036600140821     C                   ELSE
036700140821     C                   EVAL      wWHOIS_SAV = VATNOT
036800140821     C                   ENDIF
036900140821     C*
037000140821     C                   EVAL      VATTRC = 'E'
037100140821     C*
037200140821     C* x ogni tipo record 'EBCSPC' devo scaricare il buffer del file FIVAT
037300140821     C                   IF        VATNOT <> *blanks
037400140821     C                   ADD       1             §CTROKVT
037500140821     C*
037600140821     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
037700140821     C                   EVAL      VATCMR=%trim(%subst(vlrmsg:55:5))
037800140821     C                   EVAL      VATCNT = 1
037900140821     C*
038000140821     C                   WRITE     EDIVAT00
038100140821     C                   ENDIF
038200140821     C*
038300140821     C                   ENDIF
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
039500010202     C*----------------------------------------------------*
039600121115     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
039700010202     C*----------------------------------------------------*
039800020305     C     PREVAT        BEGSR
039900010202     C*
040000121115     C* Compongo il nome del membro da dare al EDIVATWR
040100010202     C                   eval      parmbr = vlrhdl
040200010202     C                   movel     'M'           parmbr
040300060113     C                   eval      parccm = vlrksc
040400010202     C                   eval      paropz = '1'
040500010202     C* Effettuo la chiamata al CLLE preposto
040600121115     C                   call(e)   'TITVEVTC'
040700010202     C                   parm                    parccm
040800010202     C                   parm                    parmbr
040900010202     C                   parm                    paropz
041000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041100010202     C                   if        %error
041200010202     C                   movel     '1'           chkcall
041300010202     C                   else
041400010202     C                   movel     '0'           chkcall
041500010202     C                   endif
041600010202     C*
041700010202     C                   ENDSR
041800000801     C*----------------------------------------------------*
041900000801     C*  CONTROLLO NUMERICITA' CAMPI
042000000801     C*----------------------------------------------------*
042100000801     C     CHKNUM        BEGSR
042200000801     C*
042300000801     C                   call(e)   'ISNUMERIC'
042400000801     C                   PARM                    PiStr            30
042500040714     C                   PARM      ','           PiDecChr          1
042600000801     C                   PARM      *ZEROS        PiVal            30 9
042700000801     C                   PARM      '0'           PiInt             1
042800000801     C                   PARM      '0'           PiNum             1
042900000801     C                   IF        %error
043000000801     C                   EVAL      PiInt=*off
043100000801     C                   ENDIF
043200000801     C*
043300000801     C                   ENDSR
043400000801     C***
043500000801
043600011113
043700011113     C*----------------------------------------------------*
043800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043900011113     C*----------------------------------------------------*
044000011113     C     CHKIMPDIV     BEGSR
044100011113     C*
044200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044300011113     C                   Z-ADD     *zeros        wrkDec            9 9
044400011113     C*
044500011113     C* Come prima cosa effettuo considerazioni sulla divisa
044600011113     C                   IF        vabIAS > *zeros
044700011113     C                   IF        vabVAS <> 'EUR'
044800011113     C                   EVAL      vabVAS =  'ITL'
044900011113     C                   ENDIF
045000011113     C                   ENDIF
045100011113     C*
045200011113     C                   IF        vabCAS > *zeros
045300011113     C                   IF        vabVCA <> 'EUR'
045400011113     C                   EVAL      vabVCA =  'ITL'
045500011113     C                   ENDIF
045600011113     C                   ENDIF
045700011113     C*
045800011113     C                   IF        vabVMD > *zeros
045900020305     C                   IF        vabVAD <> 'EUR'
046000011113     C                   EVAL      vabVAD =  'ITL'
046100011113     C                   ENDIF
046200011113     C                   ENDIF
046300011113     C*
046400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046500011113     C                   Z-ADD     vabIAS        wrkDec
046600011113     C                   IF        wrkDec > *zeros
046700011113     C                   IF        vabVAS = 'ITL'
046800011113     C                   EVAL      vabIAS = *zeros
046900011113     C                   ENDIF
047000011113     C                   ENDIF
047100011113     C*
047200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047300011113     C                   Z-ADD     vabCAS        wrkDec
047400011113     C                   IF        wrkDec > *zeros
047500011113     C                   IF        vabVCA = 'ITL'
047600011113     C                   EVAL      vabCAS = *zeros
047700011113     C                   ENDIF
047800011113     C                   ENDIF
047900011113     C*
048000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048100011113     C                   Z-ADD     vabVMD        wrkDec
048200011113     C                   IF        wrkDec > *zeros
048300011113     C                   IF        vabVAD = 'ITL'
048400011113     C                   EVAL      vabVMD = *zeros
048500011113     C                   ENDIF
048600011113     C                   ENDIF
048700011113     C*
048800011113     C                   ENDSR
048900011113     C***
049000011113
049100011113
049200000801
049300000801
049400990920      /TITLE Invio dei dati al punto operativo.
049500010202     C     invio         BEGSR
049600990920     C*
049700021113     C* 1° invio FIVAT
049800010201     C                   reset                   dscmz
049900010201     C                   move      vlrpoi        cmzdst
050000121115     C                   eval      cmzfld = 'EDIVATWR'
050100010201     C                   eval      cmzmbd = vlrhdl
050200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050300021009     C***                if        prmfir = *blanks
050400121115     C                   eval      cmzfla = 'EDIVAT0F'
050500121115     C                   eval      cmzmba = 'EDIVAT0F'
050600021009     C***                else
050700021009     C***                eval      cmzfla = prmfir
050800021009     C***                eval      cmzmba = prmfir
050900021009     C***                endif
051000010201     C                   eval      cmznrr = *zeros
051100020305     C                   move      §ctrokvt      cmznrr
051200021018     C                   eval      cmzlba = vlrfl1
051300010201     C                   call(e)   'TIS711C'
051400010201     C                   parm                    dscmz
051500010201     C                   parm      *blanks       esito
051600010205     C                   if        %error
051700010205     C                             or cmzerr = '1'
051800010205     C                             or esito  = '1'
051900010205     C                   eval      wrkesito = '3'
052000010205     C                   else
052100010201     C*
052200021113     C* 2° invio FIVAB
052300010201     C                   reset                   dscmz
052400010201     C                   move      vlrpoi        cmzdst
052500010201     C                   eval      cmzfld = vlrfou
052600010201     C                   eval      cmzmbd = vlrhdl
052700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052800021009     C***                if        prmfir = *blanks
052900121115     C                   eval      cmzfla = 'EDIVAB0F'
053000121115     C                   eval      cmzmba = 'EDIVAB0F'
053100021009     C***                else
053200021009     C***                eval      cmzfla = prmfir
053300021009     C***                eval      cmzmba = prmfir
053400021009     C***                endif
053500010201     C                   eval      cmznrr = *zeros
053600010201     C                   move      §ctrokvb      cmznrr
053700021018     C                   eval      cmzlba = vlrfl1
053800010201     C                   call(e)   'TIS711C'
053900010201     C                   parm                    dscmz
054000010201     C                   parm      *blanks       esito
054100010201     C                   if        %error
054200010201     C                             or cmzerr = '1'
054300010201     C                             or esito  = '1'
054400010201     C                   eval      wrkesito = '3'
054500010201     C                   endif
054600010205     C                   endif
054700990920     C*
054800000613     C                   ENDSR
054900000613     C***
055000070411
055100070411     C     *pssr         BEGSR
055200070411     C*
055300070411     C                   if        %open(tivin00r)
055400070411     C                   close     tivin00r
055500070411     C                   endif
055600121115     C                   if        %open(EDIVABwr)
055700121115     C                   close     EDIVABwr
055800070411     C                   endif
055900121115     C                   if        %open(EDIVATwr)
056000121115     C                   close     EDIVATwr
056100070411     C                   endif
056200070411     C*
056300070411     C* Effettuo la chiamata al CLLE preposto
056400121115     C                   call(e)   'TITVEVTC'
056500070411     C                   parm                    parccm
056600070411     C                   parm                    parmbr
056700070411     C                   parm      '2'           paropz
056800070411     C*
056900070411     C                   eval      wrkesito = '2'
057000070411     C*
057100070411     C                   seton                                        LR
057200070411     C*
057300070411     C                   ENDSR     '*CANCL'
057400070411     C***
057500070411
057600990910
057700000613     C     *inzsr        BEGSR
057800990910     C*
057900990910     C     *entry        plist
058000990920     C                   parm                    tivlrds
058100990921     C                   parm      wrkesito      esito
058200000724     C                   parm                    prmlit
058300000710     C                   parm                    prmfir
058400000613     C*
058500000830     C* CALCOLA LA DATA CORRENTE
058600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058700100722     C                   eval      datcor = %dec(%date() : *ISO)
058800121115     C                   time                    wn14             14 0
058900121115     C                   movel     wn14          oracor            6 0          *ORA
059000000830     C*
059100000613     C                   ENDSR
059200000613     C***
