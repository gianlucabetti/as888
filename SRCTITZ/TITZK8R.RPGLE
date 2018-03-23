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
002500170911     D w70             s             70    INZ(*blanks)
002600000830
002700041025     D*------------------
002800041025     D* DS REPERIMENTO NUMERATORE
002900041025     D*------------------
003000041025     D trul33ds      e ds                  inz
003100041025     D*------------------
003200041025     D* DS ARCHITETTURA
003300041025     D*------------------
003400041025     D kpjba         e ds                  inz
003500041025     D*------------------
003600990908
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004400070719     C*
004500070719     C* Esegue lancio TISI95R solo x chiusura
004600070719     C                   CLEAR                   TISI95DS
004700070719     C                   EVAL      I95TLA = 'C'
004800070719     C                   CALL      'TISI95R'
004900070719     C                   PARM                    TISI95DS
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200121115     C                   call(e)   'TITVEVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600000616     C*
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100121115     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400170911     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800121115     C                   if        not %open(EDIVABwr)
006900121115     C                   open      EDIVABwr
007000990914     C                   endif
007100121115     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600121115     C                   if        not %open(EDIVATwr)
007700121115     C                   open      EDIVATwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400100729     C*
008500100729     C                   z-add     1             wGiro             1 0
008600040910     C*
008700921023     C                   DO        *HIVAL
008800990913     C*
008900990915     C                   READ      tivin00r                               70
009000040910     C                   if        vindta > *blanks
009100000613     C                   add       1             rrnum
009200000801     C*
009300000801     C                   if        *in70 = *off
009400000801     C                             and
009500000801     C                             (vinflg = *blanks
009600000801     C                              or vinflg = '0'
009700000801     C                              or vinflg = '2')
009800000801     C*
009900000801     C                   clear                   vinmsg
010000000801     C                   eval      vinflg = '1'
010100040910     C*
010200040910     C* Eseguo routine d traduzione
010300040910     C                   exsr      impvabvat
010400040802     C*
010500010305     C                   endif
010600000905     C*
010700000905     C                   else
010800000905     C                   eval      vinflg = '1'
010900000905     C                   endif
011000000905     C*
011100000905     C  N70              update    tivin000
011200000905     C*
011300991022     C  N70              ENDdo
011400100722     C*
011500100722     C* Scarico i buffer testata ancora "in canna"
011600121115     C*
011700121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
011800170911     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:54:21))
011900121115     C                   EVAL      VABDCM = datcor
012000121115     C                   EVAL      VABDTS = datcor
012100121115     C                   EVAL      VABHMS = oracor
012200121115     C                   EVAL      VABCNT = 1
012300121115     C*
012400121115     C                   WRITE     EDIVAB00
012500010202     C*
012600010202     C                   endif
012700990910
012800990910     C* Se non ci sono record con errori ...
012900000710     C                   if        §ctrno = 0
013000990910     C* ... restituisco esito OK.
013100990921     C                   eval      wrkesito = '0'
013200990910     C                   else
013300010201     C                   if        §ctrokvb > 0
013400990921     C                   eval      wrkesito = '1'
013500000710     C                   else
013600000710     C                   eval      wrkesito = '2'
013700990910     C                   endif
013800000710     C                   endif
013900990910     C*
014000990914     C                   if        %open(tivin00r)
014100990908     C                   close     tivin00r
014200990914     C                   endif
014300121115     C                   if        %open(EDIVABwr)
014400121115     C                   close     EDIVABwr
014500990914     C                   endif
014600121115     C                   if        %open(EDIVATwr)
014700121115     C                   close     EDIVATwr
014800010201     C                   endif
014900990910     C*
015000010201     C                   if        §ctrokvb > 0
015100000724     C                             and vlrpoi <> *zeros
015200010202     C                   exsr      invio
015300990920     C                   endif
015400990920     C*
015500910830     C                   ENDSR
015600000613     C***
015700990920
015800000801     C*----------------------------------------------------*
015900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016000000801     C*----------------------------------------------------*
016100010201     C     INZVAR        BEGSR
016200000801     C*
016300040802     C                   Z-ADD     *zeros        Num5_0            5 0
016400040802     C                   MOVEL     '0'           FlgCAS            1
016500140821     C                   MOVEL     *blanks       wWHOIS_C         18
016600140821     C                   MOVEL     *blanks       wWHOIS_B         18
016700140821     C                   MOVEL     *blanks       wWHOIS_SAV       18
016800000801     C*
016900000801     C                   ENDSR
017000000801     C*----------------------------------------------------*
017100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017200000801     C*----------------------------------------------------*
017300000801     C     DEFCAM        BEGSR
017400000801     C*
017500121115     C                   CLEAR                   EDIVAB00
017600121115     C                   CLEAR                   EDIVAT00
017700020619     C* Imposto i valori di default...
017800170911     C                   Z-ADD     0053594       VABCCM
017900170911     C                   Z-ADD     0053594       VATCCM
018000170911     C                   Z-ADD     005           VABLNP
018100170911     C                   Z-ADD     005           VATLNP
018200070531     C                   Z-ADD     000           VABCTR
018300070531     C                   MOVEL     '7Q'          VABCTM
018400040823     C                   MOVEL     '1'           VABCBO
018500020619     C* ... e poi verifico se sono stati passati come parametri
018600020619     C                   IF        vlrppt > *blanks
018700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018900020619     C                   EXSR      CHKNUM
019000020619     C                   IF        PiInt=*on
019100020619     C                   Z-ADD     PiVal         VABCCM
019200020619     C                   Z-ADD     PiVal         VATCCM
019300020619     C                   ENDIF
019400040506     C                   ENDIF
019500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
019600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019700020619     C                   EXSR      CHKNUM
019800020619     C                   IF        PiInt=*on
019900020619     C                   Z-ADD     PiVal         VABLNP
020000020619     C                   Z-ADD     PiVal         VATLNP
020100040506     C                   ENDIF
020200020619     C                   ENDIF
020300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020500020619     C                   EXSR      CHKNUM
020600020619     C                   IF        PiInt=*on
020700020619     C                   Z-ADD     PiVal         VABCTR
020800040506     C                   ENDIF
020900020619     C                   ENDIF
021000060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
021100060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
021200060202     C                   ENDIF
021300020619     C                   ENDIF
021400000801     C*
021500000801     C                   ENDSR
021600000801     C*----------------------------------------------------*
021700040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
021800000801     C*----------------------------------------------------*
021900040910     C     IMPVABVAT     BEGSR
022000040910     C*
022100040910     C* Traduzione relativa ai tipi record del file del cliente
022200121115     C*
022300121115     C***
022400121115     C* ...tipo record 'EBESTD' (testata documento)
022500121115     C                   IF        %subst(vindta:1:6) = 'EBESTD'
022600121115     C*
022700121115     C* Se nn primo giro => scarico il buffer precedente
022800121115     C                   if        wGiro = 1
022900121115     C                   eval      wGiro = 2
023000121115     C                   else
023100121115     C*
023200121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
023300170911     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:54:21))
023400121115     C                   EVAL      VABDCM = datcor
023500121115     C                   EVAL      VABDTS = datcor
023600121115     C                   EVAL      VABHMS = oracor
023700121115     C                   EVAL      VABCNT = 1
023800121115     C*
023900121115     C                   WRITE     EDIVAB00
024000121115     C                   endif
024100121115     C*
024200121115     C* Resetto indicatore d anomalia sul singolo record
024300121115     C                   eval      vinflg = '1'
024400121115     C* ......inizializzazioni iniziali e formati record file Bartolini
024500121115     C                   EXSR      INZVAR
024600121115     C                   EXSR      DEFCAM
024700121115     C*
024800121115     C                   Z-ADD     *zeros        errore            1 0
024900121115     C                   MOVEL     datcor        VABAAS
025000121115     C                   MOVEL     datcor        VATAAS
025100121115     C                   MOVE      datcor        VABMGS
025200071210     C                   MOVE(P)   vlrpoi        VABFGS
025300071210     C                   MOVE(P)   vlrpoi        VATFGS
025400170911     C* ......VABNSP/VATNSP/VABRMN
025500170911     C* NSP => Stacco un numeratore da AZNUM
025600170911     C                   clear                   TRUL33DS
025700170911     C                   eval      I33OPE = *zeros
025800170911     C                   eval      I33CNU = 302
025900170911     C                   eval      I33NUM = 1
026000170911     C                   movel     TRUL33DS      KPJBU
026100170911     C                   call      'TRUL33R'
026200170911     C                   parm                    KPJBA
026300170911     C                   movel     KPJBU         TRUL33DS
026400170911     C                   if        O33ERR = *zeros
026500170911     C                   z-add     O33NRF        VABNSP
026600170911     C                   z-add     O33NRF        VATNSP
026700170911     C                   z-add     O33NRF        VABRMN
026800170911     C                   else
026900170911     C                   Z-ADD     1             errore
027000170911     C                   EVAL      vinmsg = %trimr(vinmsg)
027100170911     C                             + ' ' + 'VABNSP VATNSP VABRMN'
027200170911     C                   endif
027300170911     C* ......VABRMA
027400170911     C                   EVAL      VABRMA=%trim(%subst(vindta:25:15))
027500170911     C* ......VABRSD
027600170911     C                   EVAL      VABRSD=%trim(%subst(vindta:56:35))
027700170911     C* ......VABRD2
027800170911     C                   EVAL      VABRD2=%trim(%subst(vindta:91:35))
027900121116     C* ......VABIND
028000170911     C                   EVAL      VABIND=%trim(%subst(vindta:126:35))
028100121115     C* ......VABCAD
028200121115     C                   EVAL      VABCAD=%trim(%subst(vindta:231:9))
028300121115     C* ......VABLOD
028400121115     C                   EVAL      VABLOD=%trim(%subst(vindta:240:35))
028500110207     C* ......VABNCL
028600121115     C                   EVAL      PiStr=%trim(%subst(vindta:313:5))
028700100714     C                   EXSR      CHKNUM
028800100714     C                   IF        PiInt=*on
028900110211     C                   Z-ADD     PiVal         VABNCL
029000100714     C                   ELSE
029100100714     C                   ADD       1             errore
029200100714     C                   EVAL      vinmsg = %trimr(vinmsg)
029300100714     C                             + ' ' + 'VABNCL'
029400100714     C                   ENDIF
029500060202     C* ......VABPKB
029600121115     C                   EVAL      PiStr=%trim(%subst(vindta:318:9))
029700060202     C                   EXSR      CHKNUM
029800060202     C                   IF        PiNum=*on
029900121115     C                   EVAL      PiVal = PiVal / 1000                         gest. 3 dec.
030000070726     C                   Z-ADD(H)  PiVal         VABPKB
030100060202     C                   ELSE
030200060202     C                   ADD       1             errore
030300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
030400060202     C                             + ' ' + 'VABPKB'
030500060202     C                   ENDIF
030600070719     C* Reperisco la provincia dal CAP e dalla località
030700070719     C                   IF        VABPRD  = *blanks AND
030800070719     C                             VABCAD <> *blanks
030900070719     C                   CLEAR                   TISI95DS
031000070719     C                   EVAL      I95TCN = '3'
031100070719     C                   Z-ADD     datcor        I95DAT
031200070719     C                   EVAL      I95CAP = VABCAD
031300070719     C                   EVAL      I95LOC = VABLOD
031400070719     C                   CALL      'TISI95R'
031500070719     C                   PARM                    TISI95DS
031600070719     C                   EVAL      VABPRD = O95PRV
031700070719     C                   ENDIF
031800170911     C* ......VABLOD
031900170911     C                   EVAL      w70=%trim(%subst(vindta:336:70)) + ' ' +
032000170911     C                                 %trim(%subst(vindta:406:70)) + ' ' +
032100170911     C                                 %trim(%subst(vindta:476:70))
032200170911     C                   EVAL      VABNOT=%subst(w70:1:35)
032300170911     C                   EVAL      VABNT2=%subst(w70:36:35)
032400060202     C*
032500060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032600060202     C                   IF        FlgCAS <> '0'
032700060202     C                   IF        VABCBO = '1'
032800060202     C                   EVAL      VABCBO = '4'
032900060202     C                   ELSE
033000060202     C                   EVAL      VABCBO = '6'
033100060202     C                   ENDIF
033200060202     C                   ENDIF
033300060202     C*
033400060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033500121115     C                   EXSR      CHKIMPDIV
033600040910     C*
033700040910     C                   ENDIF
033800040910     C***
033900140821     C* ...tipo record 'EBCSTD' (dettaglio barcode segnacooli)
034000140821     C                   IF        %subst(vindta:1:6) = 'EBCSTD'
034100070719     C* Resetto indicatore d anomalia sul singolo record
034200070719     C                   eval      vinflg = '1'
034300170911     C* ......VABCCM/VATCCM
034400170911     C                   EVAL      VABCCM=%dec(%trim(%subst(vindta:15:5)):7:0)
034500170911     C                   EVAL      VATCCM=VABCCM
034600100722     C* ......VATNOT
034700170911     C                   EVAL      VATNOT=%trim(%subst(vindta:69:8))
034800170911     C*
034900170911     C                   EVAL      VATTRC = 'E'
035000170911     C*
035100170911     C* x ogni tipo record 'EBCSPC' devo scaricare il buffer del file FIVAT
035200170911     C                   IF        VATNOT <> *blanks
035300170911     C                   ADD       1             §CTROKVT
035400170911     C*
035500170911     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
035600170911     C                   EVAL      VATCMR=%trim(%subst(vlrmsg:54:21))
035700170911     C                   EVAL      VATCNT = 1
035800170911     C*
035900170911     C                   WRITE     EDIVAT00
036000170911     C                   ENDIF
036100100722     C*
036200100722     C                   ENDIF
036300140821     C***
036400010202     C*
036500000801     C* Ebbene...
036600000801     C                   ADD       1             §CTRMO
036700010201     C                   IF        errore <> *zeros
036800000801     C                   ADD       1             §CTRNO
036900000801     C                   EVAL      vinflg = '2'
037000000801     C                   ELSE
037100010201     C                   ADD       1             §CTROKVB
037200000801     C                   ENDIF
037300000801     C*
037400000801     C                   ENDSR
037500010202     C*----------------------------------------------------*
037600121115     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
037700010202     C*----------------------------------------------------*
037800020305     C     PREVAT        BEGSR
037900010202     C*
038000121115     C* Compongo il nome del membro da dare al EDIVATWR
038100010202     C                   eval      parmbr = vlrhdl
038200010202     C                   movel     'M'           parmbr
038300060113     C                   eval      parccm = vlrksc
038400010202     C                   eval      paropz = '1'
038500010202     C* Effettuo la chiamata al CLLE preposto
038600121115     C                   call(e)   'TITVEVTC'
038700010202     C                   parm                    parccm
038800010202     C                   parm                    parmbr
038900010202     C                   parm                    paropz
039000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039100010202     C                   if        %error
039200010202     C                   movel     '1'           chkcall
039300010202     C                   else
039400010202     C                   movel     '0'           chkcall
039500010202     C                   endif
039600010202     C*
039700010202     C                   ENDSR
039800000801     C*----------------------------------------------------*
039900000801     C*  CONTROLLO NUMERICITA' CAMPI
040000000801     C*----------------------------------------------------*
040100000801     C     CHKNUM        BEGSR
040200000801     C*
040300000801     C                   call(e)   'ISNUMERIC'
040400000801     C                   PARM                    PiStr            30
040500040714     C                   PARM      ','           PiDecChr          1
040600000801     C                   PARM      *ZEROS        PiVal            30 9
040700000801     C                   PARM      '0'           PiInt             1
040800000801     C                   PARM      '0'           PiNum             1
040900000801     C                   IF        %error
041000000801     C                   EVAL      PiInt=*off
041100000801     C                   ENDIF
041200000801     C*
041300000801     C                   ENDSR
041400000801     C***
041500000801
041600011113
041700011113     C*----------------------------------------------------*
041800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041900011113     C*----------------------------------------------------*
042000011113     C     CHKIMPDIV     BEGSR
042100011113     C*
042200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042300011113     C                   Z-ADD     *zeros        wrkDec            9 9
042400011113     C*
042500011113     C* Come prima cosa effettuo considerazioni sulla divisa
042600011113     C                   IF        vabIAS > *zeros
042700011113     C                   IF        vabVAS <> 'EUR'
042800011113     C                   EVAL      vabVAS =  'ITL'
042900011113     C                   ENDIF
043000011113     C                   ENDIF
043100011113     C*
043200011113     C                   IF        vabCAS > *zeros
043300011113     C                   IF        vabVCA <> 'EUR'
043400011113     C                   EVAL      vabVCA =  'ITL'
043500011113     C                   ENDIF
043600011113     C                   ENDIF
043700011113     C*
043800011113     C                   IF        vabVMD > *zeros
043900020305     C                   IF        vabVAD <> 'EUR'
044000011113     C                   EVAL      vabVAD =  'ITL'
044100011113     C                   ENDIF
044200011113     C                   ENDIF
044300011113     C*
044400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044500011113     C                   Z-ADD     vabIAS        wrkDec
044600011113     C                   IF        wrkDec > *zeros
044700011113     C                   IF        vabVAS = 'ITL'
044800011113     C                   EVAL      vabIAS = *zeros
044900011113     C                   ENDIF
045000011113     C                   ENDIF
045100011113     C*
045200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
045300011113     C                   Z-ADD     vabCAS        wrkDec
045400011113     C                   IF        wrkDec > *zeros
045500011113     C                   IF        vabVCA = 'ITL'
045600011113     C                   EVAL      vabCAS = *zeros
045700011113     C                   ENDIF
045800011113     C                   ENDIF
045900011113     C*
046000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046100011113     C                   Z-ADD     vabVMD        wrkDec
046200011113     C                   IF        wrkDec > *zeros
046300011113     C                   IF        vabVAD = 'ITL'
046400011113     C                   EVAL      vabVMD = *zeros
046500011113     C                   ENDIF
046600011113     C                   ENDIF
046700011113     C*
046800011113     C                   ENDSR
046900011113     C***
047000011113
047100011113
047200000801
047300000801
047400990920      /TITLE Invio dei dati al punto operativo.
047500010202     C     invio         BEGSR
047600990920     C*
047700021113     C* 1° invio FIVAT
047800010201     C                   reset                   dscmz
047900010201     C                   move      vlrpoi        cmzdst
048000121115     C                   eval      cmzfld = 'EDIVATWR'
048100010201     C                   eval      cmzmbd = vlrhdl
048200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
048300021009     C***                if        prmfir = *blanks
048400121115     C                   eval      cmzfla = 'EDIVAT0F'
048500121115     C                   eval      cmzmba = 'EDIVAT0F'
048600021009     C***                else
048700021009     C***                eval      cmzfla = prmfir
048800021009     C***                eval      cmzmba = prmfir
048900021009     C***                endif
049000010201     C                   eval      cmznrr = *zeros
049100020305     C                   move      §ctrokvt      cmznrr
049200021018     C                   eval      cmzlba = vlrfl1
049300010201     C                   call(e)   'TIS711C'
049400010201     C                   parm                    dscmz
049500010201     C                   parm      *blanks       esito
049600010205     C                   if        %error
049700010205     C                             or cmzerr = '1'
049800010205     C                             or esito  = '1'
049900010205     C                   eval      wrkesito = '3'
050000010205     C                   else
050100010201     C*
050200021113     C* 2° invio FIVAB
050300010201     C                   reset                   dscmz
050400010201     C                   move      vlrpoi        cmzdst
050500010201     C                   eval      cmzfld = vlrfou
050600010201     C                   eval      cmzmbd = vlrhdl
050700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050800021009     C***                if        prmfir = *blanks
050900121115     C                   eval      cmzfla = 'EDIVAB0F'
051000121115     C                   eval      cmzmba = 'EDIVAB0F'
051100021009     C***                else
051200021009     C***                eval      cmzfla = prmfir
051300021009     C***                eval      cmzmba = prmfir
051400021009     C***                endif
051500010201     C                   eval      cmznrr = *zeros
051600010201     C                   move      §ctrokvb      cmznrr
051700021018     C                   eval      cmzlba = vlrfl1
051800010201     C                   call(e)   'TIS711C'
051900010201     C                   parm                    dscmz
052000010201     C                   parm      *blanks       esito
052100010201     C                   if        %error
052200010201     C                             or cmzerr = '1'
052300010201     C                             or esito  = '1'
052400010201     C                   eval      wrkesito = '3'
052500010201     C                   endif
052600010205     C                   endif
052700990920     C*
052800000613     C                   ENDSR
052900000613     C***
053000070411
053100070411     C     *pssr         BEGSR
053200070411     C*
053300070411     C                   if        %open(tivin00r)
053400070411     C                   close     tivin00r
053500070411     C                   endif
053600121115     C                   if        %open(EDIVABwr)
053700121115     C                   close     EDIVABwr
053800070411     C                   endif
053900121115     C                   if        %open(EDIVATwr)
054000121115     C                   close     EDIVATwr
054100070411     C                   endif
054200070411     C*
054300070411     C* Effettuo la chiamata al CLLE preposto
054400121115     C                   call(e)   'TITVEVTC'
054500070411     C                   parm                    parccm
054600070411     C                   parm                    parmbr
054700070411     C                   parm      '2'           paropz
054800070411     C*
054900070411     C                   eval      wrkesito = '2'
055000070411     C*
055100070411     C                   seton                                        LR
055200070411     C*
055300070411     C                   ENDSR     '*CANCL'
055400070411     C***
055500070411
055600990910
055700000613     C     *inzsr        BEGSR
055800990910     C*
055900990910     C     *entry        plist
056000990920     C                   parm                    tivlrds
056100990921     C                   parm      wrkesito      esito
056200000724     C                   parm                    prmlit
056300000710     C                   parm                    prmfir
056400000613     C*
056500000830     C* CALCOLA LA DATA CORRENTE
056600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
056700100722     C                   eval      datcor = %dec(%date() : *ISO)
056800121115     C                   time                    wn14             14 0
056900121115     C                   movel     wn14          oracor            6 0          *ORA
057000000830     C*
057100000613     C                   ENDSR
057200000613     C***
