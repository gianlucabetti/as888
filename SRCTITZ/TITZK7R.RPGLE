000100121115      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200170908      *
000300170908      * PARTICOLARITA':
000400170908      *
000500170908      * il file contiene righe VAB/VAT (flag=A) che solo VAT (flag=B) per il codice a barre
000600170908      *
000700170908     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990910     Ftivin00r  uF   E             DISK    usropn
000900121115     FEDIVABwr  O    E             DISK    usropn
001000121115     FEDIVATwr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900070719     D tisi95ds      e ds
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900000830
003000041025     D*------------------
003100041025     D* DS REPERIMENTO NUMERATORE
003200041025     D*------------------
003300041025     D trul33ds      e ds                  inz
003400041025     D*------------------
003500041025     D* DS ARCHITETTURA
003600041025     D*------------------
003700041025     D kpjba         e ds                  inz
003800041025     D*------------------
003900990908
004000010201
004100010201
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500000613     C*
004600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004700070719     C*
004800070719     C* Esegue lancio TISI95R solo x chiusura
004900070719     C                   CLEAR                   TISI95DS
005000070719     C                   EVAL      I95TLA = 'C'
005100070719     C                   CALL      'TISI95R'
005200070719     C                   PARM                    TISI95DS
005300000613     C*
005400010202     C* Effettuo la chiamata al CLLE preposto
005500121115     C                   call(e)   'TITVEVTC'
005600010202     C                   parm                    parccm
005700010202     C                   parm                    parmbr
005800010202     C                   parm      '2'           paropz
005900000616     C*
006000010201     C                   seton                                        LR
006100990908
006200000801
006300910830     C*--------------------------------------------------------
006400121115     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006500910830     C*--------------------------------------------------------
006600040526     C     RWFILE        BEGSR
006700990910     C*
006800990914     C                   if        not %open(tivin00r)
006900990908     C                   open      tivin00r
007000990914     C                   endif
007100121115     C                   if        not %open(EDIVABwr)
007200121115     C                   open      EDIVABwr
007300990914     C                   endif
007400121115     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007500020305     C                   exsr      prevat
007600010201     C*
007700010202     C                   if        chkcall = '0'
007800010202     C*
007900121115     C                   if        not %open(EDIVATwr)
008000121115     C                   open      EDIVATwr
008100010201     C                   endif
008200990910     C*
008300010201     C                   clear                   §CTROKVB          5 0
008400020305     C                   clear                   §CTROKVT          5 0
008500000801     C                   clear                   §CTRMO            5 0
008600000801     C                   clear                   §CTRNO            5 0
008700100729     C*
008800100729     C                   z-add     1             wGiro             1 0
008900040910     C*
009000921023     C                   DO        *HIVAL
009100990913     C*
009200990915     C                   READ      tivin00r                               70
009300040910     C                   if        vindta > *blanks
009400000613     C                   add       1             rrnum
009500000801     C*
009600000801     C                   if        *in70 = *off
009700000801     C                             and
009800000801     C                             (vinflg = *blanks
009900000801     C                              or vinflg = '0'
010000000801     C                              or vinflg = '2')
010100000801     C*
010200000801     C                   clear                   vinmsg
010300000801     C                   eval      vinflg = '1'
010400040910     C*
010500040910     C* Eseguo routine d traduzione
010600040910     C                   exsr      impvabvat
010700040802     C*
010800010305     C                   endif
010900000905     C*
011000000905     C                   else
011100000905     C                   eval      vinflg = '1'
011200000905     C                   endif
011300000905     C*
011400000905     C  N70              update    tivin000
011500000905     C*
011600991022     C  N70              ENDdo
011700100722     C*
011800100722     C* Scarico i buffer testata ancora "in canna"
011900121115     C*
012000121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
012100170911     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:54:15))
012200121115     C                   EVAL      VABDCM = datcor
012300121115     C                   EVAL      VABDTS = datcor
012400121115     C                   EVAL      VABHMS = oracor
012500121115     C                   EVAL      VABCNT = 1
012600121115     C*
012700121115     C                   WRITE     EDIVAB00
012800010202     C*
012900010202     C                   endif
013000990910
013100990910     C* Se non ci sono record con errori ...
013200000710     C                   if        §ctrno = 0
013300990910     C* ... restituisco esito OK.
013400990921     C                   eval      wrkesito = '0'
013500990910     C                   else
013600010201     C                   if        §ctrokvb > 0
013700990921     C                   eval      wrkesito = '1'
013800000710     C                   else
013900000710     C                   eval      wrkesito = '2'
014000990910     C                   endif
014100000710     C                   endif
014200990910     C*
014300990914     C                   if        %open(tivin00r)
014400990908     C                   close     tivin00r
014500990914     C                   endif
014600121115     C                   if        %open(EDIVABwr)
014700121115     C                   close     EDIVABwr
014800990914     C                   endif
014900121115     C                   if        %open(EDIVATwr)
015000121115     C                   close     EDIVATwr
015100010201     C                   endif
015200990910     C*
015300010201     C                   if        §ctrokvb > 0
015400000724     C                             and vlrpoi <> *zeros
015500010202     C                   exsr      invio
015600990920     C                   endif
015700990920     C*
015800910830     C                   ENDSR
015900000613     C***
016000990920
016100000801     C*----------------------------------------------------*
016200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016300000801     C*----------------------------------------------------*
016400010201     C     INZVAR        BEGSR
016500000801     C*
016600040802     C                   Z-ADD     *zeros        Num5_0            5 0
016700040802     C                   MOVEL     '0'           FlgCAS            1
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
017800170908     C                   Z-ADD     0561889       VABCCM
017900170908     C                   Z-ADD     0561889       VATCCM
018000170908     C                   Z-ADD     056           VABLNP
018100170908     C                   Z-ADD     056           VATLNP
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
022400170908     C* ...tipo record 'A' (testata documento)
022500170908     C                   IF        %subst(vindta:49:1) = 'A'
022600121115     C*
022700121115     C* Se nn primo giro => scarico il buffer precedente
022800121115     C                   if        wGiro = 1
022900121115     C                   eval      wGiro = 2
023000121115     C                   else
023100121115     C*
023200121115     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
023300170911     C                   EVAL      VABCMR=%trim(%subst(vlrmsg:54:15))
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
025400170908     C* ......VABCCM
025500170908     C                   EVAL      PiStr=%trim(%subst(vindta:25:9))
025600170908     C                   EXSR      CHKNUM
025700170908     C                   IF        PiInt=*on
025800170908     C                   Z-ADD     PiVal         VABCCM
025900170908     C                   ELSE
026000170908     C                   ADD       1             errore
026100170908     C                   EVAL      vinmsg = %trimr(vinmsg)
026200170908     C                             + ' ' + 'VABCCM'
026300170908     C                   ENDIF
026400071210     C* ......VABNSP/VATNSP
026500170908     C*** al posto della routine REPNSP
026600170908     C**NSP => Stacco un numeratore da AZNUM
026700170908     C***                clear                   TRUL33DS
026800170908     C***                eval      I33OPE = *zeros
026900170908     C***                eval      I33CNU = 302
027000170908     C***                eval      I33NUM = 1
027100170908     C***                movel     TRUL33DS      KPJBU
027200170908     C***                call      'TRUL33R'
027300170908     C***                parm                    KPJBA
027400170908     C***                movel     KPJBU         TRUL33DS
027500170908     C***                if        O33ERR = *zeros
027600170908     C***                z-add     O33NRF        VABNSP
027700170908     C***                z-add     O33NRF        VATNSP
027800170908     C***                else
027900170908     C***                Z-ADD     1             errore
028000170908     C***                EVAL      vinmsg = %trimr(vinmsg)
028100170908     C***                          + ' ' + 'VABNSP VATNSP'
028200170908     C***                endif
028300060202     C* ......VABRMN
028400170908     C                   EVAL      PiStr=%trim(%subst(vindta:34:15))
028500060202     C                   EXSR      CHKNUM
028600060202     C                   IF        PiInt=*on
028700060202     C                   Z-ADD     PiVal         VABRMN
028800121115     C                   MOVEL     VABRMN        VABRMA
028900170908     C                   EVAL      VABNSP = %dec(%subst(vindta:37:7):7:0)
029000170908     C                   EVAL      VATNSP = VABNSP
029100060202     C                   ELSE
029200060202     C                   ADD       1             errore
029300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029400170908     C                             + ' ' + 'VABRMN VABRMA VABNSP VATNSP'
029500060202     C                   ENDIF
029600121115     C* ......VABRSD
029700170908     C                   EVAL      VABRSD=%trim(%subst(vindta:668:50))
029800121116     C* ......VABIND
029900170908     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
030000121115     C* ......VABCAD
030100170908     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
030200170908     C                   EXSR      CHKNUM
030300170908     C                   IF        PiInt=*on
030400170908     C                   Z-ADD     PiVal         Num5_0
030500170908     C                   MOVEL(P)  Num5_0        VABCAD
030600170908     C                   ELSE
030700170908     C                   ADD       1             errore
030800170908     C                   EVAL      vinmsg = %trimr(vinmsg)
030900170908     C                             + ' ' + 'VABCAD'
031000170908     C                   ENDIF
031100121115     C* ......VABLOD
031200170908     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
031300170908     C* ......VABPRD
031400170908     C                   EVAL      VABPRD=%trim(%subst(vindta:786:30))
031500170908     C* ......VABNAS
031600170908     C                   EVAL      VABNAS=%trim(%subst(vindta:210:30))
031700110207     C* ......VABNCL
031800170908     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
031900100714     C                   EXSR      CHKNUM
032000100714     C                   IF        PiInt=*on
032100110211     C                   Z-ADD     PiVal         VABNCL
032200100714     C                   ELSE
032300100714     C                   ADD       1             errore
032400100714     C                   EVAL      vinmsg = %trimr(vinmsg)
032500100714     C                             + ' ' + 'VABNCL'
032600100714     C                   ENDIF
032700060202     C* ......VABPKB
032800170908     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
032900060202     C                   EXSR      CHKNUM
033000060202     C                   IF        PiNum=*on
033100121115     C                   EVAL      PiVal = PiVal / 1000                         gest. 3 dec.
033200070726     C                   Z-ADD(H)  PiVal         VABPKB
033300060202     C                   ELSE
033400060202     C                   ADD       1             errore
033500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
033600060202     C                             + ' ' + 'VABPKB'
033700060202     C                   ENDIF
033800170908     C* ......VABVLB
033900170908     C                   EVAL      PiStr=%trim(%subst(vindta:63:7))
034000170908     C                   EXSR      CHKNUM
034100170908     C                   IF        PiNum=*on
034200170908     C                   EVAL      PiVal = PiVal / 1000                         gest. 3 dec.
034300170908     C                   Z-ADD(H)  PiVal         VABVLB
034400170908     C                   ELSE
034500170908     C                   ADD       1             errore
034600170908     C                   EVAL      vinmsg = %trimr(vinmsg)
034700170908     C                             + ' ' + 'VABVLB'
034800170908     C                   ENDIF
034900170908     b* Reperisco la provincia dal CAP e dalla località
035000070719     C                   IF        VABPRD  = *blanks AND
035100070719     C                             VABCAD <> *blanks
035200070719     C                   CLEAR                   TISI95DS
035300070719     C                   EVAL      I95TCN = '3'
035400070719     C                   Z-ADD     datcor        I95DAT
035500070719     C                   EVAL      I95CAP = VABCAD
035600070719     C                   EVAL      I95LOC = VABLOD
035700070719     C                   CALL      'TISI95R'
035800070719     C                   PARM                    TISI95DS
035900070719     C                   EVAL      VABPRD = O95PRV
036000070719     C                   ENDIF
036100060202     C*
036200060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036300060202     C                   IF        FlgCAS <> '0'
036400060202     C                   IF        VABCBO = '1'
036500060202     C                   EVAL      VABCBO = '4'
036600060202     C                   ELSE
036700060202     C                   EVAL      VABCBO = '6'
036800060202     C                   ENDIF
036900060202     C                   ENDIF
037000060202     C*
037100060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037200121115     C                   EXSR      CHKIMPDIV
037300170908     C*
037400170908     C* Eseguo la scrittura dei rcd di VAT
037500170908     C*
037600170908     C                   EVAL      VATTRC='A'
037700170908     C                   EVAL      VATNOT = %trim(%subst(vindta:748:22))
037800170908     C                   exsr      wrivat                                       => scarico VAT
037900170908     C*
038000170908     C                   EVAL      VATTRC='B'
038100170908     C                   EVAL      VATNOT = %trim(%subst(vindta:659:9))
038200170908     C                   exsr      wrivat                                       => scarico VAT
038300040910     C*
038400040910     C                   ENDIF
038500040910     C***
038600170908     C* ...tipo record 'B' (dettaglio barcode segnacooli)
038700170908     C                   IF        %subst(vindta:49:1) = 'B'
038800140821     C* Resetto indicatore d anomalia sul singolo record
038900140821     C                   eval      vinflg = '1'
039000170908     C* ......VATCCM
039100170908     C                   EVAL      PiStr=%trim(%subst(vindta:25:9))
039200170908     C                   EXSR      CHKNUM
039300170908     C                   IF        PiInt=*on
039400170908     C                   Z-ADD     PiVal         VATCCM
039500170908     C                   ELSE
039600170908     C                   ADD       1             errore
039700170908     C                   EVAL      vinmsg = %trimr(vinmsg)
039800170908     C                             + ' ' + 'VATCCM'
039900170908     C                   ENDIF
040000170908     C* ......VATNSP
040100170908     C                   EVAL      PiStr=%trim(%subst(vindta:37:7))
040200170908     C                   EXSR      CHKNUM
040300170908     C                   IF        PiInt=*on
040400170908     C                   Z-ADD     PiVal         VATNSP
040500170908     C                   ELSE
040600170908     C                   ADD       1             errore
040700170908     C                   EVAL      vinmsg = %trimr(vinmsg)
040800170908     C                             + ' ' + 'VATNSP'
040900170908     C                   ENDIF
041000140821     C* ......VATNOT
041100170908     C                   EVAL      VATNOT = %trim(%subst(vindta:56:24))
041200140821     C*
041300140821     C                   EVAL      VATTRC = 'E'
041400140821     C*
041500170908     C* x ogni tipo record 'B' devo scaricare il buffer del file FIVAT
041600170908     C                   exsr      wrivat                                       => scarico VAT
041700140821     C*
041800140821     C                   ENDIF
041900010202     C*
042000000801     C* Ebbene...
042100000801     C                   ADD       1             §CTRMO
042200010201     C                   IF        errore <> *zeros
042300000801     C                   ADD       1             §CTRNO
042400000801     C                   EVAL      vinflg = '2'
042500000801     C                   ELSE
042600010201     C                   ADD       1             §CTROKVB
042700000801     C                   ENDIF
042800000801     C*
042900000801     C                   ENDSR
043000170908     C*----------------------------------------------------*
043100170908     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
043200170908     C*----------------------------------------------------*
043300170908     C     WRIVAT        BEGSR
043400170908     C*
043500170908     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
043600170911     C                   EVAL      VATCMR=%trim(%subst(vlrmsg:54:15))
043700170908     C                   EVAL      VATCNT = 1
043800170908     C*
043900170908     C* Scrivo solo se valorizzato qualcosa
044000170908     C                   IF        VATNOT <> *blanks
044100170908     C                   ADD       1             §CTROKVT
044200170908     C                   WRITE     EDIVAT00
044300170908     C                   ENDIF
044400170908     C*
044500170908     C                   ENDSR
044600010202     C*----------------------------------------------------*
044700121115     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
044800010202     C*----------------------------------------------------*
044900020305     C     PREVAT        BEGSR
045000010202     C*
045100121115     C* Compongo il nome del membro da dare al EDIVATWR
045200010202     C                   eval      parmbr = vlrhdl
045300010202     C                   movel     'M'           parmbr
045400060113     C                   eval      parccm = vlrksc
045500010202     C                   eval      paropz = '1'
045600010202     C* Effettuo la chiamata al CLLE preposto
045700121115     C                   call(e)   'TITVEVTC'
045800010202     C                   parm                    parccm
045900010202     C                   parm                    parmbr
046000010202     C                   parm                    paropz
046100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046200010202     C                   if        %error
046300010202     C                   movel     '1'           chkcall
046400010202     C                   else
046500010202     C                   movel     '0'           chkcall
046600010202     C                   endif
046700010202     C*
046800010202     C                   ENDSR
046900000801     C*----------------------------------------------------*
047000000801     C*  CONTROLLO NUMERICITA' CAMPI
047100000801     C*----------------------------------------------------*
047200000801     C     CHKNUM        BEGSR
047300000801     C*
047400000801     C                   call(e)   'ISNUMERIC'
047500000801     C                   PARM                    PiStr            30
047600040714     C                   PARM      ','           PiDecChr          1
047700000801     C                   PARM      *ZEROS        PiVal            30 9
047800000801     C                   PARM      '0'           PiInt             1
047900000801     C                   PARM      '0'           PiNum             1
048000000801     C                   IF        %error
048100000801     C                   EVAL      PiInt=*off
048200000801     C                   ENDIF
048300000801     C*
048400000801     C                   ENDSR
048500000801     C***
048600000801
048700011113
048800011113     C*----------------------------------------------------*
048900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049000011113     C*----------------------------------------------------*
049100011113     C     CHKIMPDIV     BEGSR
049200011113     C*
049300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049400011113     C                   Z-ADD     *zeros        wrkDec            9 9
049500011113     C*
049600011113     C* Come prima cosa effettuo considerazioni sulla divisa
049700011113     C                   IF        vabIAS > *zeros
049800011113     C                   IF        vabVAS <> 'EUR'
049900011113     C                   EVAL      vabVAS =  'ITL'
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C                   IF        vabCAS > *zeros
050400011113     C                   IF        vabVCA <> 'EUR'
050500011113     C                   EVAL      vabVCA =  'ITL'
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C                   IF        vabVMD > *zeros
051000020305     C                   IF        vabVAD <> 'EUR'
051100011113     C                   EVAL      vabVAD =  'ITL'
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051600011113     C                   Z-ADD     vabIAS        wrkDec
051700011113     C                   IF        wrkDec > *zeros
051800011113     C                   IF        vabVAS = 'ITL'
051900011113     C                   EVAL      vabIAS = *zeros
052000011113     C                   ENDIF
052100011113     C                   ENDIF
052200011113     C*
052300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052400011113     C                   Z-ADD     vabCAS        wrkDec
052500011113     C                   IF        wrkDec > *zeros
052600011113     C                   IF        vabVCA = 'ITL'
052700011113     C                   EVAL      vabCAS = *zeros
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053200011113     C                   Z-ADD     vabVMD        wrkDec
053300011113     C                   IF        wrkDec > *zeros
053400011113     C                   IF        vabVAD = 'ITL'
053500011113     C                   EVAL      vabVMD = *zeros
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   ENDSR
054000011113     C***
054100011113
054200011113
054300000801
054400000801
054500990920      /TITLE Invio dei dati al punto operativo.
054600010202     C     invio         BEGSR
054700990920     C*
054800021113     C* 1° invio FIVAT
054900010201     C                   reset                   dscmz
055000010201     C                   move      vlrpoi        cmzdst
055100121115     C                   eval      cmzfld = 'EDIVATWR'
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500121115     C                   eval      cmzfla = 'EDIVAT0F'
055600121115     C                   eval      cmzmba = 'EDIVAT0F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200020305     C                   move      §ctrokvt      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010205     C                   if        %error
056800010205     C                             or cmzerr = '1'
056900010205     C                             or esito  = '1'
057000010205     C                   eval      wrkesito = '3'
057100010205     C                   else
057200010201     C*
057300021113     C* 2° invio FIVAB
057400010201     C                   reset                   dscmz
057500010201     C                   move      vlrpoi        cmzdst
057600010201     C                   eval      cmzfld = vlrfou
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000121115     C                   eval      cmzfla = 'EDIVAB0F'
058100121115     C                   eval      cmzmba = 'EDIVAB0F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700010201     C                   move      §ctrokvb      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010201     C                   if        %error
059300010201     C                             or cmzerr = '1'
059400010201     C                             or esito  = '1'
059500010201     C                   eval      wrkesito = '3'
059600010201     C                   endif
059700010205     C                   endif
059800990920     C*
059900000613     C                   ENDSR
060000000613     C***
060100070411
060200070411     C     *pssr         BEGSR
060300070411     C*
060400070411     C                   if        %open(tivin00r)
060500070411     C                   close     tivin00r
060600070411     C                   endif
060700121115     C                   if        %open(EDIVABwr)
060800121115     C                   close     EDIVABwr
060900070411     C                   endif
061000121115     C                   if        %open(EDIVATwr)
061100121115     C                   close     EDIVATwr
061200070411     C                   endif
061300070411     C*
061400070411     C* Effettuo la chiamata al CLLE preposto
061500121115     C                   call(e)   'TITVEVTC'
061600070411     C                   parm                    parccm
061700070411     C                   parm                    parmbr
061800070411     C                   parm      '2'           paropz
061900070411     C*
062000070411     C                   eval      wrkesito = '2'
062100070411     C*
062200070411     C                   seton                                        LR
062300070411     C*
062400070411     C                   ENDSR     '*CANCL'
062500070411     C***
062600070411
062700990910
062800000613     C     *inzsr        BEGSR
062900990910     C*
063000990910     C     *entry        plist
063100990920     C                   parm                    tivlrds
063200990921     C                   parm      wrkesito      esito
063300000724     C                   parm                    prmlit
063400000710     C                   parm                    prmfir
063500000613     C*
063600000830     C* CALCOLA LA DATA CORRENTE
063700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063800100722     C                   eval      datcor = %dec(%date() : *ISO)
063900121115     C                   time                    wn14             14 0
064000121115     C                   movel     wn14          oracor            6 0          *ORA
064100000830     C*
064200000613     C                   ENDSR
064300000613     C***
